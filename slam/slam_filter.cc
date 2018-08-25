#include "slam_filter.h"
#include "conf.h"
#include <math.h>
#include <chrono>
#include <numeric>
#include <fstream>

SlamParticle::SlamParticle(float weight, const vec3 &pos, const vec3 &ori)
    : weight_(weight), position_(pos), orientation_(ori) {

        unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
        gen_ = std::default_random_engine(seed);
        uniform_ = std::normal_distribution<float>(PROCESS_ACCURACY, PROCESS_NOISE);
        gaussian_ = std::normal_distribution<float>(0, PARTICLE_NOISE);
}


void SlamParticle::AddLandmark(std::string id, const vec3 &relpos, const mat3 &conv) {
    landmark_filters_.insert({{id, SlamEKF(position_ + relpos, conv, id)}});

}

void SlamParticle::UpdateLandmark(std::string id, const vec3 &relpos) {
    landmark_filters_.at(id).Update(position_ + relpos);

    std::exponential_distribution<float> exp(LANDMARK_SIGNIFICANCE);
    float a = exp(gen_);
    if (a > .5) {
        a = .5;
    }
    position_ = (1-a)*position_ + a*(landmark_filters_.at(id).xhat_ - relpos);
}

float SlamParticle::UpdateWeight(std::string id, const vec3 &relpos, float certainty) {
    SlamEKF filter = landmark_filters_.at(id);
    vec3 error = position_ - (filter.xhat_ - relpos);
    Eigen::Matrix<float, 1, 1> normalized_error = certainty*error.transpose()*filter.covs_*error;
    weight_ *= exp(-1 * normalized_error(0,0));
    return weight_;
}

void SlamParticle::UpdateParticle(const vec6 &u, float weight) {
    position_(0,0) += u(0,0)*DT*uniform_(gen_) + gaussian_(gen_)*DT*.04;
    position_(1,0) += u(1,0)*DT*uniform_(gen_) + gaussian_(gen_)*DT*.04;
    position_(2,0) = u(2,0);

    orientation_(0,0) = u(3,0);
    orientation_(1,0) = u(4,0);
    orientation_(2,0) = u(5,0);

    weight_ = weight;
}

void SlamParticle::ReinitRandom() {
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    gen_ = std::default_random_engine(seed);
}

vec3 SlamParticle::GetState() {
    return position_;
}

vec6 SlamParticle::GetState(std::string id) {
    vec6 ret;
    ret << landmark_filters_.at(id).xhat_,
           landmark_filters_.at(id).covs_.diagonal();
    return ret;
}

std::ostream& operator<<(std::ostream &os, const SlamParticle &sp) {
    os << "Particle Weight: " << sp.weight_ << std::endl;
    os << "Particle Pos: " << sp.position_.transpose() << std::endl;
    for (auto elem: sp.landmark_filters_) {
        os << "Landmark: " << elem.first << "\t" << elem.second.xhat_.transpose() << std:: endl;
    }
    return os;
}

SlamFilter::SlamFilter(int n): num_particles_(n) {
    for (int i = 0; i < n; ++i) {
        particles_.push_back(SlamParticle(1./n, vec3({0,0,0}), vec3({0,0,0})));
        weights_.push_back(1./n);
    }
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    gen_ = std::default_random_engine(seed);
}

SlamFilter::SlamFilter(int n, const vec3 &pos, const vec3 &ori): num_particles_(n) {
    for (int i = 0; i < n; ++i) {
        particles_.push_back(SlamParticle(1./n, pos, ori));
        weights_.push_back(1./n);
    }
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    gen_ = std::default_random_engine(seed);
}

void SlamFilter::Update(const vec6 &u) {
    mutex_.lock();
    float total_weight = std::accumulate(weights_.begin(), weights_.end(), 0.);
#ifdef VERBOSE
    std::cout << total_weight << std::endl;
#endif
    float offset = total_weight/(weights_.size()*100);
    float total = total_weight + offset*weights_.size();
    for (int i = 0; i < weights_.size(); ++i) {
        weights_[i] = (weights_[i] + offset)/total;
        particles_.at(i).UpdateParticle(u, weights_[i]);
    }
    mutex_.unlock();
}

void SlamFilter::Landmark(std::string id, const vec3 &relpos, const mat3 &cov) {
    mutex_.lock();
    if (landmarks_.find(id) == landmarks_.end()) {
        NewLandmark(id, relpos, cov);
    }
    else {
        UpdateLandmark(id, relpos, cov);
    }
    mutex_.unlock();
}

vec3 SlamFilter::GetState() {
    mutex_.lock();
    NormalizeWeights();
    vec3 ret(vec3::Zero());
    for (int i = 0; i < num_particles_; ++i) {
        ret += weights_[i]*particles_[i].GetState();
    }
    mutex_.unlock();
    return ret;
}

vec6 SlamFilter::GetState(std::string id) {
    if (landmarks_.find(id) == landmarks_.end()) {
        return vec6::Zero();
    }
    mutex_.lock();
    NormalizeWeights();
    vec6 ret(vec6::Zero());
    for (int i = 0; i < num_particles_; ++i) {
        ret += weights_[i]*particles_[i].GetState(id);
    }
    mutex_.unlock();
    return ret;
}

void SlamFilter::GNUPlotOut() {
    std::ofstream particle("slam/particle.dat");
    for (int i = 0; i < num_particles_; ++i) {
        vec3 position = particles_[i].GetState();
        particle << position(0,0) << " " << position(1,0) << "\n";
    }
    particle.close();

    std::ofstream landmark("slam/landmark.dat");
    for (std::string lm: landmarks_) {
        vec6 data = GetState(lm);
        landmark << data(0,0) << " " << data(1,0) << " " << data(3, 0) << " " << data(4,0) << "\n";
    }
    landmark.close();
}


std::ostream& operator<<(std::ostream &os, const SlamFilter &sf) {
    for (int i = 0; i < sf.num_particles_; ++i) {
        os << "Particle " << i << ":" << std::endl;
        os << sf.particles_.at(i);
        os << "=================" << std::endl;
    }
    return os;
}


void SlamFilter::NewLandmark(std::string id, const vec3 &relpos, const mat3 &cov) {
    for (int i = 0; i < num_particles_; ++i) {
        particles_.at(i).AddLandmark(id, relpos, cov);
    }
    landmarks_.insert(id);
}

void SlamFilter::UpdateLandmark(std::string id, const vec3 &relpos, const mat3 &cov) {
    if (landmarks_.find(id) == landmarks_.end()) {
        NewLandmark(id, relpos, cov);
        return;
    }
    float certainty = exp(-1*cov.trace()/3);
    for (int i = 0; i < particles_.size(); ++i) {
        particles_.at(i).UpdateLandmark(id, relpos);
        float w = particles_.at(i).UpdateWeight(id, relpos, certainty);
        weights_[i] = w;
    }
    NormalizeWeights();
    Resample();
}

void SlamFilter::NormalizeWeights() {
    float sum = std::accumulate(weights_.begin(), weights_.end(), 0.);
    for (int i = 0; i < weights_.size(); ++i) {
        weights_[i] /= sum;
        particles_.at(i).weight_ = weights_[i];
    }
}

void SlamFilter::Resample() {
    std::vector<SlamParticle> old_particles= particles_;
    std::vector<float> old_weights = weights_;

    std::discrete_distribution<int> multi(weights_.begin(), weights_.end());

    for (int i = 0; i < particles_.size(); ++i) {
        int sampled = multi(gen_);
        particles_[i] = old_particles[sampled];
        particles_[i].ReinitRandom();
        weights_[i] = old_weights[sampled];
    }
}
