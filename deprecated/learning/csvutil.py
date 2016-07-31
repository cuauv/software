from datetime import datetime
import csv

""" Converts time into datetime format """
def convert_time(time):
    return datetime.strptime(time, "%Y-%m-%d %H:%M:%S.%f")

""" Calculates the difference in times in terms of seconds """
def time_diff(start, end):
    return (convert_time(end) - convert_time(start)).total_seconds()

""" Loads a csv file, converting timestamps to time intervals """
def load_csv(filename):
    f = csv.reader(open(filename))
    labels = {}
    num = 0
    for label in f.next():
        labels[label] = num
        num += 1

    prev_time = None
    cur = None

    data = []
    prev = None

    for row in f:
        if prev is None:
            prev = row[0]
            continue

        features = [float(x) for x in row[1:]]
        features.insert(0,time_diff(prev, row[0]))

        prev = row[0]

        data.append(features)

    return (labels, data)

""" Gets a list of features and their classification """
def get_features(data, i):
    '''prev = data[0]
    ys = []
    for d in data[1:]:
        ys.append(d[i] - prev[i])
        prev = d
    return data[:-1], ys'''
    return data[:-1], [x[i] for x in data[1:]]
