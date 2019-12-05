#ifndef CUAUV_CONF_PARSE_H
#define CUAUV_CONF_PARSE_H

#define PARSE_BOOL(name) \
    if !(root.find(#name)) { \
        throw std::runtime_error("Expected bool " #name ", but it is not defined."); \
    } \
    bool name; \
    if (!root.find(#name)->is<bool>()) { \
        throw std::runtime_error("Expected bool " #name ", but it is not a bool."); \
    } \
    name = root.find(#name)->as<bool>();

#define PARSE_DOUBLE(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected double " #name ", but it is not defined."); \
    } \
    double name; \
    if (!root.find(#name)->is<double>()) { \
        throw std::runtime_error("Expected double " #name ", but it is not a double."); \
    } \
    name = root.find(#name)->as<double>();

#define PARSE_INTEGER(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected integer " #name ", but it is not defined."); \
    } \
    int name; \
    if (!root.find(#name)->is<int>()) { \
        throw std::runtime_error("Expected integer " #name ", but it is not an integer."); \
    } \
    name = root.find(#name)->as<int>();

#define PARSE_TWO_DOUBLES(name, d1, d2) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected array " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>() \
        || root.find(#name)->as<toml::Array>().size() != 2       \
        || !root.find(#name)->as<toml::Array>()[0].is<double>()  \
        || !root.find(#name)->as<toml::Array>()[1].is<double>()) {       \
        throw std::runtime_error("Expected vector of two doubles for " #name "."); \
    } \
    double d1 = root.find(#name)->as<toml::Array>()[0].as<double>();    \
    double d2 = root.find(#name)->as<toml::Array>()[1].as<double>();

#define VERIFY_MATRIX_ROW(name, i) \
    (root.find(#name)->as<toml::Array>()[i].is<toml::Array>()   \
     && root.find(#name)->as<toml::Array>()[i].size() == 3 \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[0].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[1].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[2].is<double>())

#define VERIFY_MATRIX4_ROW(name, i) \
    (root.find(#name)->as<toml::Array>()[i].is<toml::Array> \
     && root.find(#name)->as<toml::Array>()[i].size() == 4 \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[0].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[1].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[2].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[3].is<double>())

#define VERIFY_MATRIX6_ROW(name, i) \
    (root.find(#name)->as<toml::Array>()[i].is<toml::Array> \
     && root.find(#name)->as<toml::Array>()[i].size() == 6 \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[0].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[1].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[2].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[3].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[4].is<double>() \
     && root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[5].is<double>())

#define MAT_IJ(name, i, j) \
    root.find(#name)->as<toml::Array>()[i].as<toml::Array>()[j].as<double>()

#define PARSE_MATRIX(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix3d name; \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 3 \
        || !VERIFY_MATRIX_ROW(name, 0) \
        || !VERIFY_MATRIX_ROW(name, 1) \
        || !VERIFY_MATRIX_ROW(name, 2)) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    name << MAT_IJ(name, 0, 0), MAT_IJ(name, 0, 1), MAT_IJ(name, 0, 2), \
            MAT_IJ(name, 1, 0), MAT_IJ(name, 1, 1), MAT_IJ(name, 1, 2), \
            MAT_IJ(name, 2, 0), MAT_IJ(name, 2, 1), MAT_IJ(name, 2, 2);

#define PARSE_MATRIX4(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix<double, 4, 4> name;             \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 4 \
        || !VERIFY_MATRIX4_ROW(name, 0) \
        || !VERIFY_MATRIX4_ROW(name, 1) \
        || !VERIFY_MATRIX4_ROW(name, 2) \
        || !VERIFY_MATRIX4_ROW(name, 3)) { \
        throw std::runtime_error("Expected 4x4 matrix of doubles for " #name "."); \
    } \
    name << MAT_IJ(name, 0, 0), MAT_IJ(name, 0, 1), MAT_IJ(name, 0, 2), MAT_IJ(name, 0, 3), \
            MAT_IJ(name, 1, 0), MAT_IJ(name, 1, 1), MAT_IJ(name, 1, 2), MAT_IJ(name, 1, 3), \
            MAT_IJ(name, 2, 0), MAT_IJ(name, 2, 1), MAT_IJ(name, 2, 2), MAT_IJ(name, 2, 3), \
            MAT_IJ(name, 3, 0), MAT_IJ(name, 3, 1), MAT_IJ(name, 3, 2), MAT_IJ(name, 3, 3);

#define PARSE_MATRIX6(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix<double, 6, 6> name; \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 6 \
        || !VERIFY_MATRIX4_ROW(name, 0) \
        || !VERIFY_MATRIX4_ROW(name, 1) \
        || !VERIFY_MATRIX4_ROW(name, 2) \
        || !VERIFY_MATRIX4_ROW(name, 3) \
        || !VERIFY_MATRIX4_ROW(name, 4) \
        || !VERIFY_MATRIX4_ROW(name, 5)) { \
        throw std::runtime_error("Expected 6x6 matrix of doubles for " #name "."); \
    } \
    name << MAT_IJ(name, 0, 0), MAT_IJ(name, 0, 1), MAT_IJ(name, 0, 2), MAT_IJ(name, 0, 3), MAT_IJ(name, 0, 4), MAT_IJ(name, 0, 5), \
            MAT_IJ(name, 1, 0), MAT_IJ(name, 1, 1), MAT_IJ(name, 1, 2), MAT_IJ(name, 1, 3), MAT_IJ(name, 1, 4), MAT_IJ(name, 1, 5), \
            MAT_IJ(name, 2, 0), MAT_IJ(name, 2, 1), MAT_IJ(name, 2, 2), MAT_IJ(name, 2, 3), MAT_IJ(name, 2, 4), MAT_IJ(name, 2, 5), \
            MAT_IJ(name, 3, 0), MAT_IJ(name, 3, 1), MAT_IJ(name, 3, 2), MAT_IJ(name, 3, 3), MAT_IJ(name, 3, 4), MAT_IJ(name, 3, 5), \
            MAT_IJ(name, 4, 0), MAT_IJ(name, 4, 1), MAT_IJ(name, 4, 2), MAT_IJ(name, 4, 3), MAT_IJ(name, 4, 4), MAT_IJ(name, 4, 5), \
            MAT_IJ(name, 5, 0), MAT_IJ(name, 5, 1), MAT_IJ(name, 5, 2), MAT_IJ(name, 5, 3), MAT_IJ(name, 5, 4), MAT_IJ(name, 5, 5);

#define PARSE_VECTOR3(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected vector " #name ", but it is not an array."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 3 \
        || !root.find(#name)->as<toml::Array>()[0].is<double>() \
        || !root.find(#name)->as<toml::Array>()[1].is<double>() \
        || !root.find(#name)->as<toml::Array>()[2].is<double>()) { \
        throw std::runtime_error("Expected vector " #name ", but it is not an array of three doubles."); \
    } \
    Eigen::Vector3d name(root.find(#name)->as<toml::Array>()[0].as<double>(), \
        root.find(#name)->as<toml::Array>()[1].as<double>(), \
        root.find(#name)->as<toml::Array>()[2].as<double>());

#define PARSE_VECTOR4(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected vector of four doubles for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 4 \
        || !root.find(#name)->as<toml::Array>()[0].is<double>() \
        || !root.find(#name)->as<toml::Array>()[1].is<double>() \
        || !root.find(#name)->as<toml::Array>()[2].is<double>() \
        || !root.find(#name)->as<toml::Array>()[3].is<double>()) { \
        throw std::runtime_error("Expected vector of four doubles for " #name "."); \
    } \
    Eigen::Matrix<double, 4, 1>; \
    name << root.find(#name)->as<toml::Array>()[0].as<double>(), \
            root.find(#name)->as<toml::Array>()[1].as<double>(), \
            root.find(#name)->as<toml::Array>()[2].as<double>(), \
            root.find(#name)->as<toml::Array>()[3].as<double>();

#define PARSE_VEC6(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 6 \
        || !root.find(#name)->as<toml::Array>()[0].is<double>() \
        || !root.find(#name)->as<toml::Array>()[1].is<double>() \
        || !root.find(#name)->as<toml::Array>()[2].is<double>() \
        || !root.find(#name)->as<toml::Array>()[3].is<double>() \
        || !root.find(#name)->as<toml::Array>()[4].is<double>() \
        || !root.find(#name)->as<toml::Array>()[5].is<double>()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    Eigen::Matrix<double, 6, 1>; \
    name << root.find(#name)->as<toml::Array>()[0].as<double>(), \
            root.find(#name)->as<toml::Array>()[1].as<double>(), \
            root.find(#name)->as<toml::Array>()[2].as<double>(), \
            root.find(#name)->as<toml::Array>()[3].as<double>(), \
            root.find(#name)->as<toml::Array>()[4].as<double>(), \
            root.find(#name)->as<toml::Array>()[5].as<double>();

#define PARSE_VECTOR6(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected CWHE axes " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 6 \
        || !root.find(#name)->as<toml::Array>()[0].is<double>() \
        || !root.find(#name)->as<toml::Array>()[1].is<double>() \
        || !root.find(#name)->as<toml::Array>()[2].is<double>() \
        || !root.find(#name)->as<toml::Array>()[3].is<double>() \
        || !root.find(#name)->as<toml::Array>()[4].is<double>() \
        || !root.find(#name)->as<toml::Array>()[5].is<double>()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    vector6d name; \
    name << root.find(#name)->as<toml::Array>()[0].as<double>(), \
            root.find(#name)->as<toml::Array>()[1].as<double>(), \
            root.find(#name)->as<toml::Array>()[2].as<double>(), \
            root.find(#name)->as<toml::Array>()[3].as<double>(), \
            root.find(#name)->as<toml::Array>()[4].as<double>(), \
            root.find(#name)->as<toml::Array>()[5].as<double>();

#define PARSE_QUATERNION(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected quaterion " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<toml::Array>()) { \
        throw std::runtime_error("Expected vector of four doubles for " #name "."); \
    } \
    if (root.find(#name)->as<toml::Array>().size() != 4 \
        || !root.find(#name)->as<toml::Array>()[0].is<double>() \
        || !root.find(#name)->as<toml::Array>()[1].is<double>() \
        || !root.find(#name)->as<toml::Array>()[2].is<double>() \
        || !root.find(#name)->as<toml::Array>()[3].is<double>()) { \
        throw std::runtime_error("Expected quaternion as [w, x, y, z] for " #name "."); \
    } \
    Eigen::Quaterniond name(root.find(#name)->as<toml::Array>()[0].as<double>(), \
                            root.find(#name)->as<toml::Array>()[1].as<double>(), \
                            root.find(#name)->as<toml::Array>()[2].as<double>(), \
                            root.find(#name)->as<toml::Array>()[3].as<double>());

#define PARSE_STRING(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected string " #name ", but it is not defined."); \
    } \
    if (!root.find(#name)->is<std::string>()) { \
        throw std::runtime_error("Expected string for " #name "."); \
    } \
    std::string name(root.find(#name)->as<std::string>());

#define PARSE_DRAG_PLANES(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected drag planes " #name ", but it is not defined."); \
    } \
    drag_plane_vector name; \
    name = parse_drag_planes(*root.find(#name));

#define PARSE_THRUSTERS(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected thrusters " #name ", but it is not defined."); \
    } \
    thruster_vector name; \
    name = parse_thrusters(*root.find(#name));

#define PARSE_CAMERAS(name) \
    camera_vector name; \
    if (root.find(#name)) { \
        name = parse_cameras(*root.find(#name)); \
    }

#define PARSE_OBJECTS(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected objects " #name ", but it is not defined."); \
    } \
    std::vector<object> name; \
    name = parse_objects(*root.find(#name));

#define PARSE_SUBMARINE(name) \
    if (!root.find(#name)) { \
        throw std::runtime_error("Expected submarine " #name ", but is is not defined."); \
    } \
    sub name; \
    name = parse_submarine(*root.find(#name));

#endif // CUAUV_CONF_PARSE_H
