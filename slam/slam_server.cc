#include "slam_server.h"
#include "proto/slam_msg.pb.h"

SlamServer::SlamServer(SlamFilter *filter)
    : filter_{filter}, ctx_{1}, socket_{ctx_, ZMQ_REP} {
        socket_.bind("tcp://127.0.0.1:57411");
}

void SlamServer::Listen() {
    while (true) {
        zmq::message_t req;
        socket_.recv(&req);

        slam::SlamMsg msg;
        std::string msg_str(static_cast<char*>(req.data()), req.size());
        msg.ParseFromString(msg_str);

        if (msg.request()) {
            std::string id {msg.id().c_str()};
            if (id == "") {
                vec3 pos {filter_->GetState()};
                msg.set_m_x(pos(0,0));
                msg.set_m_y(pos(1,0));
                msg.set_m_z(pos(2,0));
            }
            else {
                vec6 state {filter_->GetState(id)};
                msg.set_m_x(state(0,0));
                msg.set_m_y(state(1,0));
                msg.set_m_z(state(2,0));
                msg.set_u_x(state(3,0));
                msg.set_u_y(state(4,0));
                msg.set_u_z(state(5,0));
            }

            std::string results;
            msg.SerializeToString(&results);

            zmq::message_t rep(results.length());
            memcpy(rep.data(), results.c_str(), results.length());
            socket_.send(rep);
        }
        else {
            std::string id {msg.id().c_str()};
            vec3 relpos {float(msg.m_x()), float(msg.m_y()), float(msg.m_z())};
            mat3 cov {vec3(float(msg.u_x()), float(msg.u_y()), float(msg.u_z())).asDiagonal()};
            filter_->Landmark(id, relpos, cov);
            zmq::message_t rep(1);
            memcpy(rep.data(), "S", 1);
            socket_.send(rep);
        }
    }
    std::cout << "Finishing Up..." << std::endl;
}
