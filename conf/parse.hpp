#ifndef CUAUV_CONF_PARSE_H
#define CUAUV_CONF_PARSE_H

#define PARSE_BOOL(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected bool " #name ", but it is not defined."); \
    } \
    bool name; \
    if (!root[#name].isBool()) { \
        throw std::runtime_error("Expected bool for " #name "."); \
    } \
    name = root[#name].asBool();

#define PARSE_DOUBLE(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected double " #name ", but it is not defined."); \
    } \
    double name; \
    if (!root[#name].isDouble()) { \
        throw std::runtime_error("Expected double for " #name "."); \
    } \
    name = root[#name].asDouble();

#define PARSE_INTEGER(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected integer " #name ", but it is not defined."); \
    } \
    int name; \
    if (!root[#name].isInt()) { \
        throw std::runtime_error("Expected integer for " #name "."); \
    } \
    name = root[#name].asInt();

#define PARSE_TWO_DOUBLES(name, d1, d2) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected array " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 2 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble()) { \
        throw std::runtime_error("Expected vector of two doubles for " #name "."); \
    } \
    double d1 = root[#name][0].asDouble(); \
    double d2 = root[#name][1].asDouble();

#define VERIFY_MATRIX_ROW(name, i) \
    (root[#name][i].isArray() && root[#name][i].size() == 3 \
    && root[#name][i][0].isDouble() \
    && root[#name][i][1].isDouble() \
    && root[#name][i][2].isDouble())

#define VERIFY_MATRIX4_ROW(name, i) \
    (root[#name][i].isArray() && root[#name][i].size() == 4 \
    && root[#name][i][0].isDouble() \
    && root[#name][i][1].isDouble() \
    && root[#name][i][2].isDouble() \
    && root[#name][i][3].isDouble())

#define VERIFY_MATRIX6_ROW(name, i) \
    (root[#name][i].isArray() && root[#name][i].size() == 6 \
    && root[#name][i][0].isDouble() \
    && root[#name][i][1].isDouble() \
    && root[#name][i][2].isDouble() \
    && root[#name][i][3].isDouble() \
    && root[#name][i][4].isDouble() \
    && root[#name][i][5].isDouble())

#define MAT_IJ(name, i, j) root[#name][i][j].asDouble()

#define PARSE_MATRIX(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix3d name; \
    if (!root[#name].isArray()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root[#name].size() != 3 \
            || !VERIFY_MATRIX_ROW(name, 0) \
            || !VERIFY_MATRIX_ROW(name, 1) \
            || !VERIFY_MATRIX_ROW(name, 2)) { \
        throw std::runtime_error("Expected 3x3 matrix of doubles for " #name "."); \
    } \
    name << MAT_IJ(name, 0, 0), MAT_IJ(name, 0, 1), MAT_IJ(name, 0, 2), \
            MAT_IJ(name, 1, 0), MAT_IJ(name, 1, 1), MAT_IJ(name, 1, 2), \
            MAT_IJ(name, 2, 0), MAT_IJ(name, 2, 1), MAT_IJ(name, 2, 2);

#define PARSE_MATRIX4(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix<double, 4, 4> name; \
    if (!root[#name].isArray()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root[#name].size() != 4 \
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
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected matrix " #name ", but it is not defined."); \
    } \
    Eigen::Matrix<double, 6, 6> name; \
    if (!root[#name].isArray()) { \
        throw std::runtime_error("Expected matrix for " #name "."); \
    } \
    if (root[#name].size() != 6 \
            || !VERIFY_MATRIX6_ROW(name, 0) \
            || !VERIFY_MATRIX6_ROW(name, 1) \
            || !VERIFY_MATRIX6_ROW(name, 2) \
            || !VERIFY_MATRIX6_ROW(name, 3) \
            || !VERIFY_MATRIX6_ROW(name, 4) \
            || !VERIFY_MATRIX6_ROW(name, 5)) { \
        throw std::runtime_error("Expected 6x6 matrix of doubles for " #name "."); \
    } \
    name << MAT_IJ(name, 0, 0), MAT_IJ(name, 0, 1), MAT_IJ(name, 0, 2), MAT_IJ(name, 0, 3), MAT_IJ(name, 0, 4), MAT_IJ(name, 0, 5), \
            MAT_IJ(name, 1, 0), MAT_IJ(name, 1, 1), MAT_IJ(name, 1, 2), MAT_IJ(name, 1, 3), MAT_IJ(name, 1, 4), MAT_IJ(name, 1, 5), \
            MAT_IJ(name, 2, 0), MAT_IJ(name, 2, 1), MAT_IJ(name, 2, 2), MAT_IJ(name, 2, 3), MAT_IJ(name, 2, 4), MAT_IJ(name, 2, 5), \
            MAT_IJ(name, 3, 0), MAT_IJ(name, 3, 1), MAT_IJ(name, 3, 2), MAT_IJ(name, 3, 3), MAT_IJ(name, 3, 4), MAT_IJ(name, 3, 5), \
            MAT_IJ(name, 4, 0), MAT_IJ(name, 4, 1), MAT_IJ(name, 4, 2), MAT_IJ(name, 4, 3), MAT_IJ(name, 4, 4), MAT_IJ(name, 4, 5), \
            MAT_IJ(name, 5, 0), MAT_IJ(name, 5, 1), MAT_IJ(name, 5, 2), MAT_IJ(name, 5, 3), MAT_IJ(name, 5, 4), MAT_IJ(name, 5, 5);

#define PARSE_VECTOR3(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 3 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble() \
            || !root[#name][2].isDouble()) { \
        throw std::runtime_error("Expected vector of three doubles for " #name "."); \
    } \
    Eigen::Vector3d name(root[#name][0].asDouble(), root[#name][1].asDouble(), root[#name][2].asDouble());

#define PARSE_VECTOR4(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 4 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble() \
            || !root[#name][2].isDouble() || !root[#name][3].isDouble()) { \
        throw std::runtime_error("Expected vector of four doubles for " #name "."); \
    } \
    Eigen::Matrix<double, 4, 1> name;\
    name << root[#name][0].asDouble(), root[#name][1].asDouble(), root[#name][2].asDouble(), root[#name][3].asDouble();

#define PARSE_VEC6(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected vector " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 6 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble() \
            || !root[#name][2].isDouble() || !root[#name][3].isDouble() \
            || !root[#name][4].isDouble() || !root[#name][5].isDouble()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    Eigen::Matrix<double, 6, 1> name; \
    name << root[#name][0].asDouble(), root[#name][1].asDouble(), root[#name][2].asDouble(), root[#name][3].asDouble(), root[#name][4].asDouble(), root[#name][5].asDouble();

#define PARSE_QUATERNION(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected quaternion " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 4 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble() \
            || !root[#name][2].isDouble() || !root[#name][3].isDouble()) { \
        throw std::runtime_error("Expected quaternion as [w, x, y, z] for " #name "."); \
    } \
    Eigen::Quaterniond name(root[#name][0].asDouble(), root[#name][1].asDouble(), \
            root[#name][2].asDouble(), root[#name][3].asDouble());

#define PARSE_STRING(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected string " #name ", but it is not defined."); \
    } \
    if (!root[#name].isString()) { \
        throw std::runtime_error("Expected string for " #name "."); \
    } \
    std::string name(root[#name].asString());

#define PARSE_DRAG_PLANES(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected drag planes " #name ", but it is not defined."); \
    } \
    drag_plane_vector name; \
    name = parse_drag_planes(root[#name]);

#define PARSE_THRUSTERS(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected thrusters " #name ", but it is not defined."); \
    } \
    thruster_vector name; \
    name = parse_thrusters(root[#name]);

#define PARSE_CAMERAS(name) \
    camera_vector name; \
    if (root.isMember(#name)) { \
        name = parse_cameras(root[#name]); \
    }

#define PARSE_VECTOR6(name) \
    if (!root.isMember(#name)) { \
        throw std::runtime_error("Expected CWHE axes " #name ", but it is not defined."); \
    } \
    if (!root[#name].isArray() || root[#name].size() != 6 \
            || !root[#name][0].isDouble() || !root[#name][1].isDouble() \
            || !root[#name][2].isDouble() || !root[#name][3].isDouble() \
            || !root[#name][4].isDouble() || !root[#name][5].isDouble()) { \
        throw std::runtime_error("Expected vector of six doubles for " #name "."); \
    } \
    vector6d name; \
    name << root[#name][0].asDouble(), root[#name][1].asDouble(), \
            root[#name][2].asDouble(), root[#name][3].asDouble(), \
            root[#name][4].asDouble(), root[#name][5].asDouble();

#define PARSE_OBJECTS(name) \
    if (!root.isMember(#name)) { \
      throw std::runtime_error("Expected objects " #name ", but it is not defined."); \
    } \
    std::vector<object> name; \
    name = parse_objects(root[#name]);

#define PARSE_SUBMARINE(name) \
    if (!root.isMember(#name)) { \
      throw std::runtime_error("Expected submarine " #name ", but is is not defined."); \
    } \
    sub name; \
    name = parse_submarine(root[#name]);

#endif // CUAUV_CONF_PARSE_H
