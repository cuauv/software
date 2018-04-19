#include "gtest/gtest.h"

#include "../packet_types/Disconnect.h"

#include <stdexcept>

using namespace cuauv::serial;

TEST(DisconnectRequestTest, RequestBytes) {
    DisconnectRequest request;
    ASSERT_EQ(request.getBytes(), std::vector<uint8_t>({0x87, 0x00, 0x00, 0x87}));
}

TEST(DisconnectRequestTest, ResponseHeader) {
    DisconnectRequest request;
    Packet::Header header;
    header.type = Packet::Type::Disconnect;
    header.len = 0;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.len = 2;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.len = 65535;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.type = Packet::Type::Hello;
    ASSERT_FALSE(request.checkResponseHeader(header));
}

TEST(DisconnectRequestTest, RequestFromBytes) {
    ASSERT_NO_THROW(DisconnectRequest request({0x87, 0x00, 0x00, 0x87}));
    ASSERT_THROW(DisconnectRequest request({0x87, 0x01, 0x00, 0x88}), std::invalid_argument);
    ASSERT_THROW(DisconnectRequest request({0x88, 0x00, 0x00, 0x88}), std::invalid_argument);
}
