#!/usr/bin/env python2
import os, sys
PATH = os.path.dirname(os.path.realpath(sys.argv[0])) + "/"

configurations = dict(
            transdec = dict(bg_picture=PATH+"Transdec.jpg",extent=(-5,130.8,0,74.6)),
            teagle = dict(bg_picture=PATH+"teagle.jpg", extent=(0,27.7,0,16.66)),
            automatic = None # Indicates to use filenames
            )

import argparse 
parser = argparse.ArgumentParser(description="""Layout Editor - Graphical display of DVL locations""")
parser.add_argument('filename', type=str, help="file containing output from marker to be viewed")
parser.add_argument('configuration', type=str, default="automatic", help="which background configuration to use.", nargs='?', choices=configurations.keys())
parser.add_argument('--save', type=str, default="", help="file name to save to", nargs='?')

args = parser.parse_args()
# Interpret no specified configuration by looking at the filename
if args.configuration == "automatic":
    args.configuration = "teagle" if "teagle" in args.filename else "transdec"
import pickle
marks = pickle.load(file(args.filename, "r"))
configuration = configurations[ args.configuration ]
save_file_name = args.save

import pylab

fig = pylab.gcf()

#Handle User Input
current_selection = None
def on_pick_event(event):
    global current_selection
    #Take selection
    if event.mouseevent.button == 1:
        ind = event.ind[-1]
        X,Y = xs[ind], ys[ind]
        current_selection = ind
fig.canvas.mpl_connect('pick_event', on_pick_event)

grab_point = None
def on_click(event):
    global grab_point
    if event.button == 3:
        if event.xdata is not None and event.ydata is not None:
            grab_point = event.xdata, event.ydata
fig.canvas.mpl_connect('button_press_event', on_click)

def on_move(event):
    #Drag selection
    global current_selection, point_plot, label_texts
    x,y = event.xdata, event.ydata
    if current_selection is not None:
        xs[current_selection] = x
        ys[current_selection] = y

        point_plot.set_data(xs,ys)

        label_texts[current_selection].set_position( (x,y) )

        pylab.draw()
    if grab_point is not None:
        x,y = event.xdata, event.ydata
        if x is not None and y is not None:
            x1,x2 = ax.get_xlim()
            y1,y2 = ax.get_ylim()
            ax.set_xlim( x1 - (x-grab_point[0]), x2 - (x-grab_point[0]))
            ax.set_ylim( y1 - (y-grab_point[1]), y2 - (y - grab_point[1]))
            pylab.draw()
fig.canvas.mpl_connect('motion_notify_event', on_move)

def on_release(event):
    global current_selection, grab_point
    if event.button == 1:
        #clear selection
        current_selection = None
    elif event.button == 3:
        grab_point = None
fig.canvas.mpl_connect('button_release_event', on_release)

def on_scroll(event):
    #Scrolling up/down for zooming
    x,y = event.xdata, event.ydata
    x1,x2 = ax.get_xlim()
    y1,y2 = ax.get_ylim()
    xp,yp = (x-x1)/(x2-x1), (y-y1)/(y2-y1)
    SCALE = 1.5
    if event.button == "up":
        width = (x2-x1)/SCALE
        height = (y2-y1)/SCALE
    elif event.button == "down":
        width = (x2-x1)*SCALE
        height = (y2-y1)*SCALE
    ax.set_xlim( x - width*xp, x + width*(1-xp) )
    ax.set_ylim( y - height*yp, y + height*(1-yp) )
    pylab.draw()
fig.canvas.mpl_connect('scroll_event', on_scroll)


#Show background image
bg = pylab.imread(configuration["bg_picture"])
pylab.subplots_adjust(left=0.05, right=0.95, top=0.95, bottom=0.05)
ax = pylab.subplot(111)
pylab.imshow(bg, extent=configuration["extent"], origin='lower')

'''
#Make the plots
labels = marks.keys()

import mission.layout.layout as layout
desired_labels = set(layout.places)

removed_labels = set(labels).difference(desired_labels)
if removed_labels:
    print "The following labels no longer appear in layout.places"
    print ", ".join(x for x in removed_labels)
    resp = raw_input("Would you like to remove them from the layout? y/N")
    if "y" in  resp or "Y" in resp:
        labels = desired_labels.intersection(labels)

new_labels = desired_labels.difference(labels)
if new_labels:
    print "Adding the following points:", ", ".join(x for x in new_labels)
avg_x = sum(marks[l][1] for l in labels)/len(labels)
avg_y = sum(marks[l][0] for l in labels)/len(labels)
for label in new_labels:
    marks[label] = (avg_x, avg_y)

labels = desired_labels

xs = [marks[l][1] for l in labels]
ys = [marks[l][0] for l in labels]

point_plot = pylab.plot(xs, ys, "ro", picker=True, pickradius=8, markersize=8)[0]

label_texts = []
for x,y,label in zip(xs,ys,labels):
    label_texts.append( pylab.text(x,y, " " + label, color='r') )
'''

pylab.grid(b=True, which='major', color='b', linestyle='-')
pylab.show()

if save_file_name == "":
    yn = raw_input("Save changes to %s? (y/N)" % args.filename)
    if "y" in yn or "Y" in yn:
        print "Saving to %s" % args.filename
        save_file_name = args.filename
    else:
        print "Exiting without saving changes"

#Output to save file
if save_file_name != "":
    save_file = file(save_file_name, "w")
    pickle.dump( dict( zip( labels, zip(ys,xs ))) , save_file )
    save_file.close()
    
