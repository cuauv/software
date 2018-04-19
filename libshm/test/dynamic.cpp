#include <gtest/gtest.h>
#include "../c/dynamic.h"
#include "../c/shm.h"

using namespace cuauv::dshm;

class DynamicTest : public testing::Test {
	public:
		DynamicTest() {
			shm_init();
		}

	protected:
		const std::vector<int> testInts{42, 85, 124};
		const std::vector<double> testDoubles{3.14159, 36.2523, -531.2234};
};


TEST_F(DynamicTest, NonDynamic) {
	for(size_t i = 0; i != testInts.size(); ++i) {
		shm_set(kalman, sensor, testInts[i]);
		shm_set(kalman, depth, testDoubles[i]);

		int sensor;
		double depth;
		shm_get(kalman, sensor, sensor);
		shm_get(kalman, depth, depth);

		EXPECT_EQ(testInts[i], sensor);
		EXPECT_EQ(testDoubles[i], depth);
	}
}

TEST_F(DynamicTest, DirectAccess) {
	auto g = newGroup("kalman");

	for(size_t i = 0; i != testInts.size(); ++i) {
		g->var("sensor")->setShm(testInts[i]);
		g->var("depth")->setShm(testDoubles[i]);

		int sensor;
		double depth;
		shm_get(kalman, sensor, sensor);
		shm_get(kalman, depth, depth);

		EXPECT_EQ(testInts[i], sensor);
		EXPECT_EQ(testDoubles[i], depth);
	}
}

TEST_F(DynamicTest, Push) {
	auto g = newGroup("kalman");

	for(size_t i = 0; i != testInts.size(); ++i) {
		g->var("sensor")->setCache(testInts[i]);
		g->var("depth")->setCache(testDoubles[i]);
		g->push();

		int sensor;
		double depth;
		shm_get(kalman, sensor, sensor);
		shm_get(kalman, depth, depth);

		EXPECT_EQ(testInts[i], sensor);
		EXPECT_EQ(testDoubles[i], depth);
	}
}

TEST_F(DynamicTest, Pull) {
	auto g = newGroup("kalman");

	for(size_t i = 0; i != testInts.size(); ++i) {
		shm_set(kalman, sensor, testInts[i]);
		shm_set(kalman, depth, testDoubles[i]);

		g->pull();

		EXPECT_EQ(testInts[i], g->var("sensor")->cachedInt());
		EXPECT_EQ(testDoubles[i], g->var("depth")->cachedDouble());
	}
}

TEST_F(DynamicTest, InitialValues) {
	for(size_t i = 0; i != testInts.size(); ++i) {
		shm_set(kalman, sensor, testInts[i]);
		shm_set(kalman, depth, testDoubles[i]);

		auto g = newGroup("kalman");
		EXPECT_EQ(testInts[i], g->var("sensor")->cachedInt());
		EXPECT_EQ(testDoubles[i], g->var("depth")->cachedDouble());
	}
}
