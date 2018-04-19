#!/usr/bin/env python2

import os, sys, argparse, pylab, json
path = os.path.dirname(os.path.realpath(sys.argv[0]))

parser = argparse.ArgumentParser(description = 'Graphical view of marked locations')
parser.add_argument('--filename', type = str, help = 'File from which to read marks (defaults to marks.json)', default = 'marks.json')
parser.add_argument('--locale', type = str, help = 'Locale to map (defaults to CUAUV_LOCALE)', default = os.environ['CUAUV_LOCALE'])
parser.add_argument('--tags', action = 'store_true', help = 'Store objects from tag name instead of object name')
parser.add_argument('-l', '--from-locale', action = 'store_true', help = 'Load from locale configuration instead of mark file')

args = parser.parse_args()

if args.from_locale:
  fn = os.path.join(os.environ['CUAUV_SOFTWARE'], 'conf', os.environ['CUAUV_LOCALE'] + '.conf')
  locale = json.load(open(fn))
  marks = [{'position': obj['initial_position'][:3], 'tag': None, 'object': obj['name']} for obj in locale['objects']]
else:
  marks = json.load(open(args.filename))

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

bg = pylab.imread(os.path.join(os.path.join(path, 'images'), args.locale + '.jpg'))
pylab.subplots_adjust(left=0.05, right=0.95, top=0.95, bottom=0.05)
ax = pylab.subplot(111)

bg_width = 27.7
bg_height = 16.66
corner_offset_x = -5.5
corner_offset_y = -bg_height + 3

extent = (corner_offset_x, bg_width + corner_offset_x,
          corner_offset_y, bg_height + corner_offset_y)
pylab.imshow(bg, extent=extent, origin='lower')

xs = [m['position'][1] for m in marks]
ys = [m['position'][0] for m in marks]
reverse = {ind: marks[ind]['tag' if args.tags else 'object'] for ind in range(len(marks))}
labels = [m['object'] + ('' if m['tag'] is None else ' (' + m['tag'] + ')') for m in marks]

point_plot = pylab.plot(xs, ys, 'ro', picker = 10.0, pickradius = 8, markersize = 8)[0]
label_texts = []
for x, y, label in zip(xs, ys, labels):
  label_texts.append(pylab.text(x, y, ' ' + label, color = 'r'))

pylab.grid(b=True, which='major', color='b', linestyle='-')

try:
  pylab.show()
except KeyboardInterrupt:
  pass

save = raw_input('Exiting! Would you like to save all updated marks to the locale file [y/n] ? ')
if save == 'y':
  fn = os.path.join(os.environ['CUAUV_SOFTWARE'], 'conf', os.environ['CUAUV_LOCALE'] + '.conf')
  locale = json.load(open(fn))
  for ind in range(len(xs)):
    for x in locale['objects']:
      if x['name'] == reverse[ind]:
        x['initial_position'][0] = ys[ind]
        x['initial_position'][1] = xs[ind]
        print('Wrote position of object {} to locale file.'.format(reverse[ind]))
  out = open(fn, 'w')
  json.dump(locale, out, sort_keys = True, indent = 4, separators = (', ',': '))
  out.close()
