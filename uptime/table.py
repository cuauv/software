#!/usr/bin/env python
# table.py by Zeth 

"""This module creates reStructuredText tables from a basic Python datatype.
One use is writing to a file, another is converting to HTML with Docutils;
a third use is handing on to Django's markup module. 
You may be able to find other uses that I do not know about.

The input data needs to be a basic matrix using tuples and/or lists.

If you have a dictionary, you can convert it into a list of tuples,
as shown in the example below.

>>> mydict = {'tuple': 'An immutable sequence.', 
...           'list': 'A mutable sequence.',}
>>> mylist = list(mydict.iteritems())
>>> mytable = Table(mylist)
>>> print mytable.create_table(first_row_is_header = False)
+-------+------------------------+
| list  | A mutable sequence.    |
+-------+------------------------+
| tuple | An immutable sequence. |
+-------+------------------------+
<BLANKLINE>

"""

__author__ = 'Zeth'
__url__ = 'http://commandline.org.uk/code/table.py'

class Table(object):
    """A valid reStructuredText table.
    
    The following shows example usage.
    
    >>> mydata = (("Website", "URL", "Began"),
    ... ("Commandline Warriors", "http://commandline.org.uk", "2005"),
    ... ("Zeth's homepage", "http://zeth.co.uk", "2000"),
    ... ("Pixelise", "http://pixelise.org", "2008"),
    ... ("Python West Midlands", "http://pywm.eu", "2006"))
    >>> mytable = Table(mydata)
    >>> print mytable.create_table()
    +----------------------+---------------------------+-------+
    | Website              | URL                       | Began |
    +======================+===========================+=======+
    | Commandline Warriors | http://commandline.org.uk | 2005  |
    +----------------------+---------------------------+-------+
    | Zeth's homepage      | http://zeth.co.uk         | 2000  |
    +----------------------+---------------------------+-------+
    | Pixelise             | http://pixelise.org       | 2008  |
    +----------------------+---------------------------+-------+
    | Python West Midlands | http://pywm.eu            | 2006  |
    +----------------------+---------------------------+-------+
    <BLANKLINE>
    """

    def __init__(self, data):
        """Data is a list/tuple of lists/tuples.
        
        The __init__ method is used for initialisation, you do not have to
        care about it unless you are doing something particularly dynamic.
        
        The .widths attribute contains a list of the column widths, 
        this is used to make the table beautiful.
        
        >>> Recent_British_History = (
        ... ("Event", "Year",),
        ... ("Peace in Europe", "1945"),
        ... ("Universal Free Healthcare", "1948"),
        ... ("Coronation of Elizabeth II", "1953"),
        ... ("The Rolling Stones' First Number One", "1964"),
        ... ("England wins Football World Cup", "1966"),
        ... ("European Community", "1973"),
        ... ("Falklands Crisis", "1982"),
        ... ("Good Friday Agreement", "1998"),
        ... ("London Bombings", "2005"),
        ... ("London Olympics", "2012")
        ... )
        >>> history_table = Table(Recent_British_History)
        >>> print history_table.widths
        [36, 4]
        
        """
        self.data = data
        self.widths = len(data[0]) * [0]
        # Calculate the column widths.
        for row in self.data:
            i = 0
            for cell in row:
                if len(cell) > self.widths[i]:
                    self.widths[i] = len(cell)
                i += 1
                
    def create_table(self, first_row_is_header = True):
        """Make a table.
        
        This is the main method worth caring about. It returns a string
        with a reStructuredText table, as the following example shows.
        
        >>> world_cup_wins = ( ("Country", "Wins"), ("Brazil", "5"),
        ... ("Italy", "4"), ("Germany", "3"), ("Argentina", "2"), 
        ... ("Uruguay", "2"), ("England", "1"), ("France", "1"),)
        >>> world_cup_table = Table(world_cup_wins)
        >>> print world_cup_table.create_table()
        +-----------+------+
        | Country   | Wins |
        +===========+======+
        | Brazil    | 5    |
        +-----------+------+
        | Italy     | 4    |
        +-----------+------+
        | Germany   | 3    |
        +-----------+------+
        | Argentina | 2    |
        +-----------+------+
        | Uruguay   | 2    |
        +-----------+------+
        | England   | 1    |
        +-----------+------+
        | France    | 1    |
        +-----------+------+
        <BLANKLINE>   
        
        """
        table = ""
        rows = list(self.data)
        row_div = self._divider()
        table += row_div
        # Create header if needed
        if first_row_is_header:
            table += self._create_row(rows.pop(0))
            table += self._divider(True)
        # Add in the rows    
        for row in rows:
            table += self._create_row(row)
            table += row_div                     
        return table

    def _divider(self, header = False):
        """Make a table divider.
        
        You only need to care about this if you want to do something 
        particularly dynamic.
        
        The ._divider method will give you a vertical divider, as shown
        in the example below. The 'header' argument determines what kind of
        line is used.
        
        >>> batting_records = (("Name", "Score", "Place", "Year"),
        ... ("Brian Lara", "501", "Birmingham", "1994"),
        ... ("Hanif Mohammad", "499", "Karachi", "1959"),
        ... ("Don Bradman", "452", "Syndey", "1930 "))
        >>> cricket_table = Table(batting_records)
        >>> print cricket_table._divider()
        +----------------+-------+------------+-------+
        <BLANKLINE>
        >>> print cricket_table._divider(header = True)
        +================+=======+============+=======+
        <BLANKLINE>

        """
        if header:
            line = "="
        else:
            line = "-"
        div_sub = "+%s" * len(self.widths) + "+\n"
        div_tup = tuple([((width + 2) * line) for width in self.widths])
        return div_sub % div_tup
    
    def _create_row(self, row):    
        """Make a row.
        
        You only need to care about this if you want to do something 
        particularly dynamic.
        
        The row is a list containing each piece of data, you can retrieve 
        this from the dataset as shown in the example below, or you can
        provide new data.
        
        >>> absolute_zero = (("0 K", "-273.15 C"),)
        >>> cold_table = Table(absolute_zero)
        >>> print cold_table._create_row(cold_table.data[0])
        | 0 K | -273.15 C |
        <BLANKLINE>

        """
        row_list = []
        row_space = []
        i = 0         
        for cell in row:
            # Calculate the required spacing for each cell
            row_space.append((self.widths[i] - len(cell)) * " ")
            # Add the cell data and the space to the row list
            row_list.append(cell)
            row_list.append(row_space[i])
            i += 1
        # Put the rows together
        row_sub = "| %s%s " * len(self.widths) + "|\n"
        return row_sub % tuple(row_list)

def main():
    """Run doctests when called directly."""
    import doctest
    doctest.testmod()

# start the ball rolling
if __name__ == "__main__": 
    main()


