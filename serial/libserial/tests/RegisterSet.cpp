#include "gtest/gtest.h"

#include "../Register.h"

#include <stdexcept>

using namespace cuauv::serial;

class RegisterSetTest : public ::testing::Test {
    protected:
        virtual void SetUp() {
            sparseSet.insert(20);
            sparseSet.insert(48);
            sparseSet.insert(118);
            sparseSet.insert(53);

            denseSet.insert(20);
            denseSet.insert(25);
            denseSet.insert(23);
            denseSet.insert(24);
            denseSet.insert(27);
            denseSet.insert(28);
            denseSet.insert(29);

            contiguousSet.insertRange(12, 30);
        }

        RegisterSet sparseSet;
        RegisterSet denseSet;
        RegisterSet emptySet;
        RegisterSet contiguousSet;
};

TEST_F(RegisterSetTest, RangeSmaller) {
    ASSERT_FALSE(sparseSet.rangeIsSmaller());
    ASSERT_TRUE(denseSet.rangeIsSmaller());
    ASSERT_TRUE(contiguousSet.rangeIsSmaller());
    emptySet.rangeIsSmaller(); // check that the program does not crash
}

TEST_F(RegisterSetTest, NumRegisters) {
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(denseSet.numRegisters(), 7);
    ASSERT_EQ(emptySet.numRegisters(), 0);
    ASSERT_EQ(contiguousSet.numRegisters(), 30);
}

TEST_F(RegisterSetTest, Contains) {
    ASSERT_TRUE(sparseSet.contains(48));
    ASSERT_FALSE(sparseSet.contains(57));
    ASSERT_FALSE(sparseSet.contains(57));
}

TEST_F(RegisterSetTest, InsertNew) {
    ASSERT_FALSE(sparseSet.contains(23));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    sparseSet.insert(23);
    ASSERT_EQ(sparseSet.numRegisters(), 5);
    ASSERT_TRUE(sparseSet.contains(23));
}

TEST_F(RegisterSetTest, InsertExisting) {
    ASSERT_TRUE(sparseSet.contains(48));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    sparseSet.insert(48);
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_TRUE(sparseSet.contains(48));
}

TEST_F(RegisterSetTest, InsertRange) {
    ASSERT_FALSE(sparseSet.contains(54));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    sparseSet.insertRange(50, 5);
    ASSERT_EQ(sparseSet.numRegisters(), 8);
    ASSERT_TRUE(sparseSet.contains(54));
    ASSERT_FALSE(sparseSet.contains(55));
}

TEST_F(RegisterSetTest, IterationOrdered) {
    auto it = denseSet.begin();
    auto last = *it;
    it++;
    for (; it != denseSet.end(); it++) {
        ASSERT_GT(*it, last);
        last = *it;
    }
}

TEST_F(RegisterSetTest, EmptyIteration) {
    for (auto reg : emptySet) {
        (void) reg; // suppress unused variable error
        FAIL() << "Empty set should be empty";
    }
}

TEST_F(RegisterSetTest, ToRange) {
    auto range = sparseSet.toRange();
    ASSERT_EQ(range.startRegister(), 20);
    ASSERT_EQ(range.numRegisters(), 99);

    range = denseSet.toRange();
    ASSERT_EQ(range.startRegister(), 20);
    ASSERT_EQ(range.numRegisters(), 10);

    range = emptySet.toRange();
    ASSERT_EQ(range.numRegisters(), 0);

    range = contiguousSet.toRange();
    ASSERT_EQ(range.startRegister(), 12);
    ASSERT_EQ(range.numRegisters(), 30);
}
