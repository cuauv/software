#include <fstream>
#include <gtest/gtest.h>

#include "fakedev/FakeDevice.h"
#include <serial/libserial/DeviceInfo.h>

#include "../../device_list.h"
#include "../../config.h"

using namespace cuauv::serial;

class DeviceTest : public testing::Test {
	public:
		DeviceTest(): m_config{std::make_shared<Config>("seriald.toml")}, m_devList{m_config} {
			std::ifstream protoFile("hydrophones.pb");
			if (!protoFile.is_open()) {
				throw std::runtime_error("Proto file not found");
			}

			std::stringstream buffer;
			buffer << protoFile.rdbuf();
			std::string protoStr = buffer.str();
			protoFile.close();

			auto devInfo = std::make_shared<DeviceInfo>(0x00, 0x00, protoStr);
			m_fakeDev = std::make_shared<FakeDevice>("fifo:hydrophones", devInfo);

			m_fakeDev->start();
		}

		~DeviceTest() {
			m_fakeDev->stop();
		}

	protected:
		std::shared_ptr<Config> m_config;
		DeviceList m_devList;
		std::shared_ptr<FakeDevice> m_fakeDev;
};

TEST_F(DeviceTest, Basic) {
}
