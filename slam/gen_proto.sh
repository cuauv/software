# TODO work this into ninja
mkdir -p proto
protoc slam_msg.proto --cpp_out=proto --python_out=proto
