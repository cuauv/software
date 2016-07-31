#include <vector>
#define DIML 100 // Probability map length, single dimension
#define NSF 0.5 // North scale factor.
#define ESF 0.5 // East scale factor.
#define TDC 0.1 // Time decay intensity.
#define MAXDEV 2 // Observation filtering maximum deviation from expected.

template<typename T>
struct VE {
    T value;
    T error;
};

class Event{
    private:
        double eunixtime;
        std::function<double(double, double)> euf;
    public:
        Event(std::function<double(double, double)>, double unixtime) { euf = uf; eunixtime = unixtime; }
        double *getBPM() { double bpm [DIML][DIML] = {}; return bpm; } // TODO
};

class Object{
    private:
        double pmap [DIML][DIML];
        std::vector<Event> events;
    public:
        Object() {} // TODO
        int addEvent(Event e) { events.push_back(e); }
        void move(double d_n, double d_e, double err) {} // TODO
        VE<double> getCentroid(); // north, then east, with error
};

class State{
    private:
        double derr [DIML];
        double derrscale;
        double herr [DIML];
        double herrscale;
        std::vector<Object> objects;
    public:
        State() {} // TODO
        int addObject() { objects.push_back(Object()); return objects.size(); }
        void hObs(int index, double heading, double err) {} // TODO
        void dObs(int index, double distance, double err) {} // TODO
        void move(double d_n, double d_e, double err) { objects.at(0).move(d_n, d_e, err); }
        VE<double> *getPosition() { VE<double> pos [2]; return pos; } // TODO
        VE<double> *getMeasurementErrors() { VE<double> merr [2]; return merr; } // TODO
};

