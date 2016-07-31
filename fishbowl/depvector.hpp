#ifndef CUAUV_SIM_DEPVECTOR_H
#define CUAUV_SIM_DEPVECTOR_H

#include <utility>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <algorithm>
#include <unordered_set>

namespace cuauv {
namespace fishbowl {

class uint32_pair_hasher {
public:
    size_t operator() (const std::pair<uint32_t, uint32_t>& x) const
    {
        size_t h1 = std::hash<uint32_t>()(x.first);
        size_t h2 = std::hash<uint32_t>()(x.second);
        return h1 ^ (h2 << 1);
    }
};

// A [depvector] contains a [vector<std::pair<K, V>>] and tracks dependencies
// of [(K, V)] pairs on [(type, id)] dependency tuples. It provides a [remove]
// operation for entries in the contained vector that cleans up dependency
// tracking information, and a [removed] operation for dependencies that removes
// from the contained vector dependent entries.
template<typename K, typename V>
class depvector {
public:
    typedef std::pair<K, V> P;
    typedef std::vector<P> T;

    depvector(T& x);

    void depend(K i, uint32_t deptype, uint32_t depid);
    void undepend(K i, uint32_t deptype, uint32_t depid);
    std::unordered_set<K> removed(uint32_t deptype, uint32_t depid);
    void remove(K i);

private:
    T& x;

    // deps[(t1, i1)] = {a, b, ...}
    // means that [a], [b], ... depend on [(t1, i1)].
    std::unordered_map<std::pair<uint32_t, uint32_t>, std::unordered_set<K>, uint32_pair_hasher> deps;

    // ideps[i] = {(t1, i1), ...}
    // Means the value at key [i] in the contained vector has a dependency
    // [(t1, i1)].
    std::unordered_map<K, std::unordered_set<std::pair<uint32_t, uint32_t>, uint32_pair_hasher>> ideps;
};

template<typename K, typename V>
depvector<K, V>::depvector(depvector<K, V>::T& x)
    : x(x)
{
}

template<typename K, typename V>
void depvector<K, V>::depend(K i, uint32_t deptype, uint32_t depid)
{
    auto p = std::make_pair(deptype, depid);
    deps[p].insert(i);
    ideps[i].insert(p);

}


template<typename K, typename V>
void depvector<K, V>::undepend(K i, uint32_t deptype, uint32_t depid)
{
    if (ideps.count(i) == 0) {
        throw std::invalid_argument("No dependencies registered for given [i].");
    }
    auto p = std::make_pair(deptype, depid);
    if (ideps[i].count(p) == 0) {
        throw std::invalid_argument("[deptype], [depid] not registered as a dependency for given [i].");
    }
    ideps[i].erase(p);
    deps[p].erase(i);
}

template<typename K, typename V>
std::unordered_set<K> depvector<K, V>::removed(uint32_t deptype, uint32_t depid)
{
    std::vector<K> rs;
    auto p = std::make_pair(deptype, depid);
    if (deps.count(p) == 0) {
        // See http://stackoverflow.com/a/26949099 .
        // Thanks Debian and C++ standards committee.
        return {{}};
    }
    std::unordered_set<K> ds = deps[p];
    for (auto i : ds) {
        remove(i);
    }
    deps.erase(p);
    return ds;
}

template<typename K, typename V>
void depvector<K, V>::remove(K i)
{
    bool removed = false;
    for (auto it = x.begin(); it != x.end(); it++) {
        if ((*it).first == i) {
            removed = true;
            x.erase(it);
            break;
        }
    }
    if (!removed) {
        throw std::invalid_argument("No value with key [i].");
    }
    for (auto& x : ideps[i]) {
        deps[x].erase(i);
        if (deps[x].size() == 0) {
            deps.erase(x);
        }
    }
    ideps.erase(i);
}

} // namespace fishbowl
} // namespace cuauv

#endif // CUAUV_SIM_DEPVECTOR_H
