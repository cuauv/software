#include "gtest/gtest.h"

#include "../Register.h"

#include <stdexcept>

using namespace cuauv::serial;

class BoundRegisterSetTest : public ::testing::Test {
    protected:
        virtual void SetUp() {
            sparseSet.insert(20, 36);
            sparseSet[48] = 21;
            sparseSet.insert(118, 0);
            sparseSet.insert(53, 12);

            denseSet.insert(20, 6);
            denseSet[25] = 3;
            denseSet.insert(23, 200);
            denseSet[24] = 49;
            denseSet.insert(27, 123);
            denseSet.insert(28, 118);
            denseSet[29] = 80;

            contiguousSet[9] = 0;
            contiguousSet[5] = 2;
            contiguousSet[8] = 118;
            contiguousSet[6] = 123;
            contiguousSet[10] = 0;
            contiguousSet[7] = 42;

            vectorSet = BoundRegisterSet({5, 23, 48, 6}, {27, 48, 2, 0});

            indexSet = RegisterSet({12, 48, 2, 6, 221});
            indexedSet = BoundRegisterSet(indexSet, {12, 42, 22, 4, 0});

            emptyVectorSet = BoundRegisterSet(std::vector<uint8_t>(), {});
            emptyIndexedSet = BoundRegisterSet(RegisterSet(), {});
        }

        BoundRegisterSet sparseSet;
        BoundRegisterSet denseSet;
        BoundRegisterSet vectorSet;
        BoundRegisterSet indexedSet;
        BoundRegisterSet contiguousSet;

        BoundRegisterSet emptySet;
        BoundRegisterSet emptyVectorSet;
        BoundRegisterSet emptyIndexedSet;

        RegisterSet indexSet;
};

TEST_F(BoundRegisterSetTest, Contains) {
    ASSERT_TRUE(sparseSet.contains(48));
    ASSERT_FALSE(sparseSet.contains(120));

    ASSERT_TRUE(denseSet.contains(25));
    ASSERT_FALSE(denseSet.contains(22));

    ASSERT_TRUE(contiguousSet.contains(8));
    ASSERT_FALSE(contiguousSet.contains(11));

    ASSERT_TRUE(vectorSet.contains(23));
    ASSERT_FALSE(vectorSet.contains(50));

    ASSERT_TRUE(indexedSet.contains(2));
    ASSERT_FALSE(indexedSet.contains(18));
}

TEST_F(BoundRegisterSetTest, NumRegisters) {
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(denseSet.numRegisters(), 7);
    ASSERT_EQ(contiguousSet.numRegisters(), 6);
    ASSERT_EQ(vectorSet.numRegisters(), 4);
    ASSERT_EQ(indexedSet.numRegisters(), 5);
    ASSERT_EQ(emptySet.numRegisters(), 0);
    ASSERT_EQ(emptyVectorSet.numRegisters(), 0);
    ASSERT_EQ(emptyIndexedSet.numRegisters(), 0);
}

TEST_F(BoundRegisterSetTest, RangeIsSmaller) {
    ASSERT_FALSE(sparseSet.rangeIsSmaller());
    ASSERT_TRUE(denseSet.rangeIsSmaller());
    ASSERT_TRUE(contiguousSet.rangeIsSmaller());
    ASSERT_FALSE(vectorSet.rangeIsSmaller());
    ASSERT_FALSE(indexedSet.rangeIsSmaller());
    ASSERT_NO_THROW(emptySet.rangeIsSmaller());
    ASSERT_NO_THROW(emptyVectorSet.rangeIsSmaller());
    ASSERT_NO_THROW(emptyIndexedSet.rangeIsSmaller());
}

TEST_F(BoundRegisterSetTest, InsertNew) {
    ASSERT_FALSE(sparseSet.contains(55));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    sparseSet.insert(55, 123);
    ASSERT_TRUE(sparseSet.contains(55));
    ASSERT_EQ(sparseSet.numRegisters(), 5);
    ASSERT_EQ(sparseSet[55], 123);
}

TEST_F(BoundRegisterSetTest, InsertExisting) {
    ASSERT_TRUE(sparseSet.contains(53));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(sparseSet[53], 12);
    sparseSet.insert(53, 123);
    ASSERT_TRUE(sparseSet.contains(53));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(sparseSet[53], 123);
}

TEST_F(BoundRegisterSetTest, InsertBracketNew) {
    ASSERT_FALSE(sparseSet.contains(55));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    sparseSet[55] = 123;
    ASSERT_TRUE(sparseSet.contains(55));
    ASSERT_EQ(sparseSet.numRegisters(), 5);
    ASSERT_EQ(sparseSet[55], 123);
}

TEST_F(BoundRegisterSetTest, InsertBracketExisting) {
    ASSERT_TRUE(sparseSet.contains(53));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(sparseSet[53], 12);
    sparseSet[53] = 123;
    ASSERT_TRUE(sparseSet.contains(53));
    ASSERT_EQ(sparseSet.numRegisters(), 4);
    ASSERT_EQ(sparseSet[53], 123);
}

TEST_F(BoundRegisterSetTest, ToBoundRange) {
    BoundRegisterRange range = sparseSet.toBoundRange();
    ASSERT_EQ(range.startRegister(), 20);
    ASSERT_EQ(range.numRegisters(), 99);
    ASSERT_EQ(range.at(53), 12);

    range = vectorSet.toBoundRange();
    ASSERT_EQ(range.startRegister(), 5);
    ASSERT_EQ(range.numRegisters(), 44);
    ASSERT_EQ(range.at(23), 48);

    range = indexedSet.toBoundRange();
    ASSERT_EQ(range.startRegister(), 2);
    ASSERT_EQ(range.numRegisters(), 220);
    ASSERT_EQ(range.at(48), 4);

    range = emptySet.toBoundRange();
    ASSERT_EQ(range.numRegisters(), 0);
    range = emptyVectorSet.toBoundRange();
    ASSERT_EQ(range.numRegisters(), 0);
    range = emptyIndexedSet.toBoundRange();
    ASSERT_EQ(range.numRegisters(), 0);
}

TEST_F(BoundRegisterSetTest, IterationKeyOrdered) {
    auto it_k = sparseSet.key_begin();
    auto it_v = sparseSet.value_begin();

    auto last_key = *it_k;
    ASSERT_EQ(sparseSet[*it_k], *it_v);
    it_k++; it_v++;
    for (; it_k != sparseSet.key_end(); it_k++, it_v++) {
        ASSERT_GT(*it_k, last_key);
        ASSERT_EQ(sparseSet[*it_k], *it_v);
        last_key = *it_k;
    }
}
