#include "gtest/gtest.h"

#include "../DeviceInfo.h"
#include "../Variable.h"

#include <iostream>
#include <fstream>
#include <stdexcept>

using namespace cuauv::serial;

class DeviceInfoTest : public ::testing::Test {
    protected:
        virtual void SetUp() {
            std::ifstream protoFile;

			auto cuauvSof = getenv("CUAUV_SOFTWARE");
			if(cuauvSof == nullptr) {
				throw std::runtime_error("CUAUV_SOFTWARE not set");
			}
			auto protoDir = std::string(cuauvSof) + "serial/libserial/tests/proto/";
            
            protoFile.open(protoDir + "test.pb", std::ios::binary);
            if (!protoFile.is_open()) {
                throw std::runtime_error("Can't find test.pb");
            }
            std::stringstream buffer;
            buffer << protoFile.rdbuf();
            testCfg = buffer.str();
            protoFile.close();

            protoFile.open(protoDir + "empty.pb", std::ios::binary);
            if (!protoFile.is_open()) {
                throw std::runtime_error("Can't find empty.pb");
            }
            buffer = std::stringstream();
            buffer << protoFile.rdbuf();
            emptyCfg = buffer.str();
            protoFile.close();
        }

        std::string testCfg;
        std::string emptyCfg;
};

TEST_F(DeviceInfoTest, EmptyCfg) {
    DeviceInfo info(0x38, 0x5A, emptyCfg);
    ASSERT_EQ(0, info.writeVariables().size());
    ASSERT_EQ(0, info.pollGroups().size());
    ASSERT_EQ("empty", info.name());
    ASSERT_EQ("fake_proto", info.type());
    ASSERT_EQ(0x38, info.protocolVersion());
    ASSERT_EQ(0x5A, info.resetFlags());
}

TEST_F(DeviceInfoTest, TestCfg) {
    DeviceInfo info(0xFF, 0x00, testCfg);
    ASSERT_EQ(3, info.writeVariables().size());
    ASSERT_EQ(4, info.pollGroups().size());
    ASSERT_EQ("test", info.name());
    ASSERT_EQ("fake_proto", info.type());
    ASSERT_EQ(0xFF, info.protocolVersion());
    ASSERT_EQ(0x00, info.resetFlags());
}

TEST_F(DeviceInfoTest, NullCfg) {
    ASSERT_ANY_THROW(DeviceInfo info(0x00, 0x00, ""));
}

TEST_F(DeviceInfoTest, CorruptCfg) {
    auto corruptCfg = testCfg;
    corruptCfg[1] = 0x42;
    ASSERT_ANY_THROW(DeviceInfo info(0x00, 0x00, corruptCfg));
}

TEST_F(DeviceInfoTest, PollGroups) {
    DeviceInfo info(0x00, 0x00, testCfg);

    ASSERT_EQ(2, info.pollGroups().at(0).size());
    ASSERT_EQ(1, info.pollGroups().at(100).size());
    ASSERT_EQ(0, info.pollGroups().at(250).size());
    ASSERT_EQ(2, info.pollGroups().at(65535).size());
}

// Variable tests come in Variable.cpp
