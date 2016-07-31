#pragma once

#include <map>
#include <vector>
#include <iostream>

namespace cuauv {
namespace serial {
namespace cli {

/**
 * @brief A memory image which can be uploaded to a device
 */
class MemoryImage {
    public:
        typedef uint16_t address_t;
        typedef uint8_t byte_t;

        /**
         * Creates an empty memory image
         */
        MemoryImage();

        /**
         * Adds a new chunk of data to the memory image
         *
         * @throws std::invalid_argument if any part of the chunk overlaps with an existing one
         *
         * @param base the base address of the chunk
         *
         * @param data the data to add
         */
        void addChunk(address_t base, const std::vector<byte_t>& data);

        /**
         * Gets the set of all chunks in this memory image
         *
         * @returns a map, where keys are the base addresses of chunks, 
         *  and values are the data held in those chunks
         */
        std::map<address_t, std::vector<byte_t>> getChunks() const;

        /**
         * Merges two memory images together
         *
         * @param other the other image to merge into this one
         */
        void merge(const MemoryImage& other);

        std::map<address_t, std::vector<byte_t>> asPages(unsigned int pageSize) const;

    private:
        std::map<address_t, std::vector<byte_t>> m_chunks;
};

}}} // end namespace cuauv::serial::cli
