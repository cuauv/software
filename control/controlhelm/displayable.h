#ifndef __DISPLAYABLE_H
#define __DISPLAYABLE_H

class Displayable {
    public:
        virtual void update() = 0;
        virtual void redraw() = 0;
        virtual ~Displayable() {};
};

#endif
