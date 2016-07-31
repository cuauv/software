#include "gtest/gtest.h"

#include "../packet_types/Reset.h"

#include <stdexcept>

using namespace cuauv::serial;

TEST(ResetRequestTest, RequestBytes) {
    ResetRequest request;
    ASSERT_EQ(request.getBytes(), std::vector<uint8_t>({0x81, 0x00, 0x00, 0x81}));
}

TEST(ResetRequestTest, ResponseHeader) {
    ResetRequest request;
    Packet::Header header;
    header.type = Packet::Type::Reset;
    header.len = 0;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.len = 2;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.len = 65535;
    ASSERT_FALSE(request.checkResponseHeader(header));
    header.type = Packet::Type::Disconnect;
    ASSERT_FALSE(request.checkResponseHeader(header));
}

TEST(ResetRequestTest, RequestFromBytes) {
    ASSERT_NO_THROW(ResetRequest request({0x81, 0x00, 0x00, 0x81}));
    ASSERT_THROW(ResetRequest request({0x81, 0x01, 0x00, 0x82}), std::invalid_argument);
    ASSERT_THROW(ResetRequest request({0x82, 0x00, 0x00, 0x82}), std::invalid_argument);
}
