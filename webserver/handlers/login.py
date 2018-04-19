from webserver import APIHandler

class LoginHandler(APIHandler):

    def post(self):
        try:
            user = self.get_body_argument("user")
            password = self.get_body_argument("password")
            if not self.set_user(user):
                self.respond_failure("Username not found!")
            else:
                self.respond_success("Logged in!")

        except MissingArgumentError:
            self.respond_failure("Username/password not supplied")

