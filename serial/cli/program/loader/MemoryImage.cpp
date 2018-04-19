#include "MemoryImage.h"

#include <algorithm>
#include <iostream>

namespace cuauv {
namespace serial {
namespace cli {

MemoryImage::MemoryImage()
{
}

std::map<MemoryImage::address_t, std::vector<MemoryImage::byte_t>> MemoryImage::getChunks() const {
    return m_chunks;
}

void MemoryImage::addChunk(address_t base, const std::vector<byte_t>& data) {
    // If we don't have any chunks yet, just insert this one
    if (m_chunks.empty()) {
        m_chunks[base] = data;
        return;
    }

    auto nextIt = m_chunks.begin();
    auto prevIt = nextIt++;
    address_t data_end = base + data.size();
    while (nextIt != m_chunks.end() && nextIt->first < data_end) {
        prevIt = nextIt;
        nextIt++;
    }

    address_t prev_end = prevIt->first + prevIt->second.size();
    if (prevIt->first > data_end) {
        // This should go before the first chunk
        // slight hack: change prevIt to equal nextIt
        nextIt = prevIt;
    } else if (prevIt->first >= base) {
        // The first chunk starts partway through this chunk
        throw std::invalid_argument("Chunk overlaps with existing chunk");
    } else {
        // Check that the chunk does not overlap with the previous one
        // The previous loop guarantees that it doesn't overlap with the next one
        if (prev_end > base) {
            throw std::invalid_argument("Chunk overlaps with existing chunk");
        }
    }

    // Need to save previous base, since if we delete nextIt all iterators will
    // be invalidated
    address_t prev_base = prevIt->first;
    std::vector<byte_t> expanded_data = data;
    // Check if the next chunk should merge with this one
    if (nextIt != m_chunks.end() && nextIt->first == data_end) {
        // Merge the next chunk in to this one
        expanded_data.insert(expanded_data.end(), nextIt->second.begin(), nextIt->second.end());
        m_chunks.erase(nextIt);
    }

    if (prev_end == base) {
        // The new chunk should merge with the previous
        m_chunks[prev_base].insert(m_chunks[prev_base].end(), expanded_data.begin(), expanded_data.end());
    } else {
        // Make this a new chunk
        m_chunks[base] = expanded_data;
    }
}

void MemoryImage::merge(const MemoryImage& other) {
    for (auto chunk : other.m_chunks) {
        addChunk(chunk.first, chunk.second);
    }
}

std::map<MemoryImage::address_t, std::vector<MemoryImage::byte_t>> MemoryImage::asPages(unsigned int pageSize) const {
    std::map<address_t, std::vector<byte_t>> pages;

    std::vector<byte_t> currentPage(pageSize, 0xFF);
    address_t curPageAddr = 0;
    bool pageNotEmpty = false;

    for (auto chunk : m_chunks) {
        size_t n = 0;
        while (n < chunk.second.size()) {
            address_t nextByteAddr = chunk.first + n;
            if (nextByteAddr >= curPageAddr + pageSize) {
                // finalize the current page, start a new one
                if (pageNotEmpty) {
                    pages.insert(std::make_pair(curPageAddr, currentPage));
                }
                currentPage = std::vector<byte_t>(pageSize, 0xFF);
                curPageAddr = nextByteAddr - (nextByteAddr % pageSize);
                pageNotEmpty = false;
            }
            auto pageOffset = nextByteAddr - curPageAddr;
            size_t i = 0;
            while (n < chunk.second.size() && pageOffset + i < pageSize) {
                currentPage[pageOffset + i] = chunk.second[n];
                n++; i++;
                pageNotEmpty = true;
            }
        }
    }

    if (pageNotEmpty) {
        pages.insert(std::make_pair(curPageAddr, currentPage));
    }
    return pages;
}

}}} // end namespace cuauv::serial::cli
