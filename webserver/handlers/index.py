from webserver import BaseHandler

class IndexHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("index.html"))
