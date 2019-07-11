
class SqlClass:
    """
    Magic class that wraps MySQL
    """
    #This class method  must be overloaded by superclasses
    #it should perform any necessary table setup
    @classmethod
    def table_setup(cls):
        raise NotImplementedError("Class %s must implement a table_setup method" % cls.__name__)

    @classmethod
    def remove_table(cls):
        cls.c.execute("DROP TABLE IF EXISTS %s" % cls.table_name)
        cls.commit()

    @classmethod
    def turn_off_commits(cls):
        cls.always_commit = False

    @classmethod
    def turn_on_commits(cls):
        cls.always_commit = True

    @classmethod
    def link_sqlite(cls, db):
        cls.conn = db.conn
        cls.turn_on_commits()
        cls.c = db.c
        cls.db = db
        cls.table_setup()

    @classmethod
    def commit(cls):
        if cls.always_commit:
            cls.conn.commit()

    def __init__(self):
        pass

    ban_keys = ["id"]
    e_cache = {} #Element cache ensures no duplicate instances

    @classmethod
    def __get_table_keys__(cls):
        cls.c.execute("PRAGMA table_info(%s)" % cls.table_name)
        return [x['name'] for x in cls.c.fetchall()]

    @classmethod
    def get_keys_from_instance(cls, instance):
        keys = cls.__get_table_keys__()
        for k in cls.ban_keys:
            keys.remove(k)

        d = {}
        for k in keys:
            d[k] = getattr(instance, k)
        return d

    @classmethod
    def new(cls, **args):
        keys = cls.__get_table_keys__()
        for k in cls.ban_keys:
            keys.remove(k)
        for k in keys:
            if k not in args.keys():
                raise ValueError("SQL requires key %s be provided" % str(k))
        if set(args.keys()) != set(keys):
            print("akeys %s, keys %s" % (str(set(args.keys())), str(set(keys))))
            raise ValueError("Too many parameters")
        cls.c.execute("INSERT INTO %s(%s) values (%s)" % 
            (cls.table_name, ','.join(args.keys()), ','.join(['?' for k in args.keys()])), tuple(args.values())) 

        v = cls()
        for k in args.keys():
            setattr(v, k, args[k])
        v.id = cls.c.lastrowid
        cls.commit()
        cls.e_cache[(cls.__name__, v.id, cls.db.filename)] = v
        return v

    @classmethod
    def add(cls, instance):
        cls.new(**cls.get_keys_from_instance(instance))

    @classmethod
    def get_all(cls, filter_str=""):
        cls.c.execute("SELECT * FROM %s %s" % (cls.table_name, filter_str))
        rows = cls.c.fetchall()
        elements = []
        for r in rows:
            index = cls.__name__, r['id'], cls.db.filename
            if index in cls.e_cache:
                v = cls.e_cache[index]
            else:
                v = cls()
                cls.e_cache[index] = v
            #Set all variables
            for k in r.keys():
                setattr(v, k, r[k])
            elements.append(v)
        return elements


    def update(self):
        keys = self.__get_table_keys__()
        for k in self.ban_keys:
            keys.remove(k)
        vals = [getattr(self, k) for k in keys]
        self.c.execute("UPDATE %s SET %s WHERE id = %s" % (self.table_name, ','.join("%s = ?" % k for k in keys), self.id), vals)
        self.commit()

    def remove(self):
        self.c.execute("DELETE FROM %s WHERE id = '%s'" % (self.table_name, self.id))
        if (self.__class__.__name__, self.id, self.db.filename) in self.e_cache:
            del self.e_cache[(self.__class__.__name__, self.id, self.db.filename)]
        self.commit()

