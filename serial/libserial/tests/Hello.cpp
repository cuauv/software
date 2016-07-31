#include "gtest/gtest.h"

#include "../packet_types/Hello.h"

#include <stdexcept>

using namespace cuauv::serial;

TEST(HelloRequestTest, RequestBytes) {
    HelloRequest request;
    ASSERT_EQ(request.getBytes(), std::vector<uint8_t>({0x80, 0x00, 0x00, 0x80}));
}

TEST(HelloRequestTest, ResponseHeader) {
    HelloRequest request;
    Packet::Header header;
    header.type = Packet::Type::Hello;
    header.len = 0;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.len = 2;
    ASSERT_TRUE(request.checkResponseHeader(header));
    header.len = 65535;
    ASSERT_TRUE(request.checkResponseHeader(header));
    header.type = Packet::Type::Reset;
    ASSERT_FALSE(request.checkResponseHeader(header));
}

TEST(HelloRequestTest, RequestFromBytes) {
    ASSERT_NO_THROW(HelloRequest request({0x80, 0x00, 0x00, 0x80}));
    ASSERT_THROW(HelloRequest request({0x80, 0x01, 0x00, 0x81}), std::invalid_argument);
    ASSERT_THROW(HelloRequest request({0x81, 0x00, 0x00, 0x81}), std::invalid_argument);
}
