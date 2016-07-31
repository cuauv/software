import facebook

YOUR_API_KEY = '34fc282cb0e81f80666828404745b31c'
YOUR_SECRET_KEY = '1aabc0c42c09234bf77b16ec7c0df825'
fb = facebook.Facebook(YOUR_API_KEY, YOUR_SECRET_KEY)
auth_token = fb.auth.createToken()
print "Make sure to say \'Stay logged in\'!"
fb.login()
session = fb.auth.getSession()
if session['expires']:
    print "You did not specify to stay logged in!"
else:
    print "SESSION_KEY = ", session['session_key']
    print "SESSION_SECRET = ", session['secret']
