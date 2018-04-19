from datetime import datetime
import os, jinja2
import tornado.web

class Jinja2TemplateRendering:

    def __init__(self, application):
        self.application = application
        _app_settings = application.settings
        _jinja_config = dict(
            extensions  = ['jinja2.ext.autoescape', 'jinja2.ext.with_'],
            auto_reload = _app_settings.get('autoreload', False),
            loader      = jinja2.FileSystemLoader(_app_settings.get('template_path', 'templates'))
        )
        self.environment = jinja2.Environment(**_jinja_config)

    def render_template(self, template_name, **kwargs):
        def _ctx_processor():
            rv = dict(
                request        = self.application.request,
                settings       = self.application.settings,
                reverse_url    = self.application.reverse_url,
                static_url     = self.application.static_url,
                environ        = os.environ.get,
                datetime       = datetime
            )
            return rv
        ctx = _ctx_processor()
        ctx.update(kwargs)

        template = self.environment.get_template(template_name)
        return template.render(ctx)

USER_LEVELS = {
    "guest": 0,
    "elevated": 1
}

class BaseHandler(tornado.web.RequestHandler):

    def __init__(self, *args, **kwargs):
        super(BaseHandler, self).__init__(*args, **kwargs)
        self._jinja2_rendering = Jinja2TemplateRendering(self)

    def get_current_user(self):
        user = self.get_secure_cookie("user")
        if not user:
            self.set_secure_cookie("user", "guest")
            user = self.get_secure_cookie("user")
        return user

    def is_elevated_user(self):
        user = get_current_user()
        if USER_LEVELS.has_key(user):
            return USER_LEVELS[user] >= USER_LEVELS["elevated"]
        else:
            return False

    def set_user(self, user):
        if USER_LEVELS.has_key(user):
            self.set_secure_cookie("user", user)
            return True
        else:
            self.set_secure_cookie("user", "guest")
            return False

    def render_template(self, template_name, template_values={}):
        return self._jinja2_rendering.render_template(template_name, **template_values)

class APIHandler(BaseHandler):

    def respond_success(self, msg):
        self.write({ "success": 1, "msg": msg })

    def respond_failure(self, msg):
        self.write({ "success": 0, "msg": msg })
