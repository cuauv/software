#ifndef _STL_READ_H
#define _STL_READ_H

#include <string>
#include <fstream>
#include <sstream>

// This class is used to parse STL files.
// See en.wikipedia.org/wiki/STL_(file_format)

class STLFile {
  public:
    ~STLFile() { close(); }
    // Opens the STL file with the provided filename.
    int open(const std::string& filename);
    // Places the normal and 3 vertices corresponding to the next triangle in out.
    // Returns false if there are no more triangles.
    bool get_next_triangle(float *out);
    // Closes the STL file.
    void close();

  private:
    bool get_next_triangle_ascii(float *out);
    bool get_next_triangle_binary(float *out);

    std::ifstream file;
    std::stringstream iss;
    uint32_t N; // Number of triangles in the model
    uint32_t current = 0;

    bool ascii;
};

#endif
