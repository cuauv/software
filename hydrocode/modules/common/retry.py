def retry(func, e):
    def wrapped(*args, **kwargs):
        while True:
            try:
                return func(*args, **kwargs)
            except e:
                pass

    return wrapped