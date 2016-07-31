// ASLAM Interface Specification
// Written by Christopher Goes (cwg46@cornell.edu)
// Last Updated 11 April 2014

#ifndef __ASLAM_H_
#define __ASLAM_H_

#include<cstring>
#include<vector>
#include<unordered_map>
#include<memory>

using std::vector;
using std::string;
using std::unordered_map;
using std::shared_ptr;

#define DIMX 500
#define DIMY 500

class Function {
    public:
        virtual double call(double n, double e) = 0;
};

// 2-dimensional matrix
class Matrix {
        public:
                Matrix();
                Matrix(double initialValue);
                Matrix(double xMax, double yMax, double initialValue); // Create a matrix with specified initial value
                //Matrix(const Matrix& other); // Copy constructor
                //~Matrix(); // Destructor
                void update(shared_ptr<Function> f); // Update a matrix with a function
                shared_ptr<Matrix> operator*(shared_ptr<Matrix> m); // Multiply two matrices element-wise
                shared_ptr<Matrix> operator^(double p); // Raise a matrix to a power
                void operator*=(Matrix m);
                //Matrix& operator=(const Matrix& m); // Assignment operator
                void centroid();
                void error();
                void normalize();
                
                vector<vector<double>> vals;
                double xScale, yScale, centn, cente, err;
};

class Event {
    public:
        double time;
        shared_ptr<Matrix> eum;
};

class Object {
    public:
        shared_ptr<Matrix> pmap;
        vector<shared_ptr<Event> > events;
        double centn, cente, err;
        double centn_offset = 0.;
        double cente_offset = 0.;
};

template<typename T>
struct tuple {
    T v1;
    T v2;
};

template<typename T>
struct triple {
    T v1;
    T v2;
    T v3;
};

// 2-dimension specific.
class State {
        public:
                unordered_map<string, shared_ptr<Object>> objects;
                State(double initN, double initE, double initErr);
                void addObject(string identifier, double n, double e, double err);
                bool hObs(string o1i, string o2i, double heading, double err);
                bool dObs(string o1i, string o2i, double distance, double err);
                void move(double deltaX, double deltaY, double err);
                void update();
                double* getEstHeadingError();
                double* getEstDistanceError();
                double getHeading(string o1i, string o2i);
                double getDistance(string o1i, string o2i);
                double getN(string o1i, string o2i);
                double getE(string o1i, string o2i);
        private:
                void doCovariance(string obji, double prevn, double preve, double err);
                double getEstOffsetError(vector<triple<double> > data);
                double getEstLinearError(vector<triple<double> > data);
                unordered_map<string, unordered_map<string, double> > covariance;
                vector<triple<double> > distanceErrors;
                vector<triple<double> > headingErrors;
};
#endif

#ifdef __CTYPES_
extern "C" {
    State* s_new(double iN, double iE, double iErr) { return new State(iN, iE, iErr); }
    void s_delete(State* s) { delete s; }
    void s_addObject(State* s, char* id, double iN, double iE, double iErr) { s->addObject(id, iN, iE, iErr); }
    void s_hObs(State* s, char* o1id, char* o2id, double heading, double err) { s->hObs(o1id, o2id, heading, err); }
    void s_dObs(State* s, char* o1id, char* o2id, double distance, double err) { s->dObs(o1id, o2id, distance, err); }
    void s_move(State* s, double dX, double dY, double err) { s->move(dX, dY, err); }
    void s_update(State* s) { s->update(); }
    double* s_getEstHeadingError(State* s) { return s->getEstHeadingError(); }
    double* s_getEstDistanceError(State* s) { return s->getEstDistanceError(); }
    double s_getHeading(State* s, char* o1i, char* o2i) { return s->getHeading(o1i, o2i); }
    double s_getDistance(State* s, char* o1i, char* o2i) { return s->getDistance(o1i, o2i); }
    double s_getN(State* s, char* o1i, char* o2i) { return s->getN(o1i, o2i); }
    double s_getE(State* s, char* o1i, char* o2i) { return s->getE(o1i, o2i); }
}
#endif
