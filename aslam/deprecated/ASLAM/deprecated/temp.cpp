#include<vector>
#include<iostream>
#include<memory>

#define DEFAULT_XMAX 50
#define DEFAULT_YMAX 50
#define DIMX 500
#define DIMY 500

using namespace std;

class Matrix {
        public:
                Matrix();
                Matrix(double initialValue);
                Matrix(double xMax, double yMax, double initialValue); // Create a matrix with specified initial value
                //Matrix(const Matrix& other); // Copy constructor
                //~Matrix(); // Destructor
//                void update(shared_ptr<Function> f); // Update a matrix with a function
                shared_ptr<Matrix> operator*(shared_ptr<Matrix> m); // Multiply two matrices element-wise
                shared_ptr<Matrix> operator^(double p); // Raise a matrix to a power
                //Matrix& operator=(const Matrix& m); // Assignment operator
                vector<double> centroid();
                double error();
                double get(double x, double y) { return vals[x][y]; }
//                vector<vector<double>> vals;
        private:
                vector<vector<double>> vals;
                void normalize();
                double xScale;
                double yScale;
                double centn, cente;
};

Matrix::Matrix() { //Matrix(1.); 
         //xScale = xMax / DIMX;
         //yScale = yMax / DIMY;
         for (int x = 0; x < DIMX; x++) { vals[x].resize(DIMY); for (int y = 0; y < DIMY; y++) { vals[x][y] = 1.; } }
}

Matrix::Matrix(double initialValue) { 
     vals.resize(DIMX);
         //xScale = xMax / DIMX;
         //yScale = yMax / DIMY;
         for (int x = 0; x < DIMX; x++) { vals[x].resize(DIMY); for (int y = 0; y < DIMY; y++) { vals[x][y] = initialValue; } }
    //Matrix(DEFAULT_XMAX, DEFAULT_YMAX, initialValue); 
}

Matrix::Matrix(double xMax, double yMax, double initialValue) {
    vals.resize(DIMX);
    //xScale = xMax / DIMX;
    //yScale = yMax / DIMY;
    for (int x = 0; x < DIMX; x++) { vals[x].resize(DIMY); for (int y = 0; y < DIMY; y++) { vals[x][y] = initialValue; } }
    cout << vals[0][0] << endl;
    cout << vals[0][0] << endl;
    string s;
    cin >> s;
}

int main() {
    Matrix m = Matrix(0., 0., 1.);
    cout << m.get(0, 0) << endl;
}
