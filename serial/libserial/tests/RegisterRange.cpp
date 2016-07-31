#include "gtest/gtest.h"

#include "../Register.h"

#include <stdexcept>

using namespace cuauv::serial;

TEST(RegisterRange, Size) {
    RegisterRange range(5, 30);
    ASSERT_EQ(range.startRegister(), 5);
    ASSERT_EQ(range.numRegisters(), 30);
}

TEST(RegisterRange, ZeroSize) {
    RegisterRange range(32, 0);
    ASSERT_EQ(range.startRegister(), 32);
    ASSERT_EQ(range.numRegisters(), 0);
}

TEST(RegisterRange, BoundsCheck) {
    ASSERT_THROW(RegisterRange range(250, 6), std::invalid_argument);
    ASSERT_THROW(RegisterRange range(0, 256), std::invalid_argument);
}

class BoundRegisterRangeTest : public ::testing::Test {
    public:
        BoundRegisterRangeTest() : filledRange(0, 0), emptyRange(0, 0),
            zeroFilledRange(0, 0), zeroEmptyRange(0, 0) {}
    protected:
        virtual void SetUp() {
            regs = {22, 39, 0, 186, 5, 33, 48};
            filledRange = BoundRegisterRange(5, regs);
            emptyRange = BoundRegisterRange(27, 5);
            zeroFilledRange = BoundRegisterRange(31, {});
            zeroEmptyRange = BoundRegisterRange(23, 0);
        }

        std::vector<uint8_t> regs;
        BoundRegisterRange filledRange;
        BoundRegisterRange emptyRange;
        BoundRegisterRange zeroFilledRange;
        BoundRegisterRange zeroEmptyRange;
};

TEST_F(BoundRegisterRangeTest, ConstructorBoundsCheck) {
    ASSERT_THROW(BoundRegisterRange range(253, regs), std::invalid_argument);
    ASSERT_THROW(BoundRegisterRange range(252, 20), std::invalid_argument);
}

TEST_F(BoundRegisterRangeTest, FilledSize) {
    ASSERT_EQ(filledRange.startRegister(), 5);
    ASSERT_EQ(filledRange.numRegisters(), regs.size());
}

TEST_F(BoundRegisterRangeTest, EmptySize) {
    ASSERT_EQ(emptyRange.startRegister(), 27);
    ASSERT_EQ(emptyRange.numRegisters(), 5);
}

TEST_F(BoundRegisterRangeTest, ZeroFilledSize) {
    ASSERT_EQ(zeroFilledRange.startRegister(), 31);
    ASSERT_EQ(zeroFilledRange.numRegisters(), 0);
}

TEST_F(BoundRegisterRangeTest, ZeroEmptySize) {
    ASSERT_EQ(zeroEmptyRange.startRegister(), 23);
    ASSERT_EQ(zeroEmptyRange.numRegisters(), 0);
}

TEST_F(BoundRegisterRangeTest, FilledAccess) {
    ASSERT_EQ(filledRange.at(5), 22);
    ASSERT_EQ(filledRange.at(9), 5);
}

TEST_F(BoundRegisterRangeTest, FilledBoundsCheck) {
    ASSERT_THROW(filledRange.at(4), std::out_of_range);
    ASSERT_THROW(filledRange.at(12), std::out_of_range);
}

TEST_F(BoundRegisterRangeTest, EmptyBoundsCheck) {
    ASSERT_THROW(emptyRange.at(26), std::out_of_range);
    ASSERT_THROW(emptyRange.at(32), std::out_of_range);
}

TEST_F(BoundRegisterRangeTest, FilledModify) {
    ASSERT_EQ(filledRange.at(6), 39);
    filledRange.at(6) = 42;
    ASSERT_EQ(filledRange.at(6), 42);
}

TEST_F(BoundRegisterRangeTest, EmptyModify) {
    emptyRange.at(28) = 42;
    ASSERT_EQ(emptyRange.at(28), 42);
}

TEST_F(BoundRegisterRangeTest, FilledIterate) {
    auto it1 = regs.begin();
    auto it2 = filledRange.begin();
    for (; it1 != regs.end(); it1++, it2++) {
        ASSERT_EQ(*it1, *it2);
    }
    ASSERT_EQ(it2, filledRange.end());
}
