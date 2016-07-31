import shm

"""
Common definitions for classification / training
"""

SVC_FILENAME = "svc_bins_data.pkl"

                # Name         # SVC index     # shm output group
bin_options = { "Squid"      : (0,             shm.shape_classifier_squid),
                "Crab"       : (1,             shm.shape_classifier_crab),
                "Jellyfish"  : (2,             shm.shape_classifier_jellyfish),
                "Mothership" : (3,             shm.shape_classifier_mothership),
}

#erverse mapping
svc_index_to_name = dict((v[0], k) for k,v in bin_options.items())

# Vision groups containing possible shape results
vision_groups = [shm.shape_results_1,
                shm.shape_results_2,
                shm.shape_results_3,
                shm.shape_results_4]

def get_feature_vector_from_group(g):
    """
    Map a vision group "g" to a feature vector, that is, a list
    of integers / floats
    """
    return [g.h0.get(),
            g.h1.get(),
            g.h2.get(),
            g.h3.get(),
            g.h4.get(),
            g.h5.get(),
            g.h6.get(),
            g.aspect.get(),
            g.pf_rect.get(),
            g.pf_hull.get(),
            g.pf_bin.get(),
            g.holes.get(),
            g.contour_count.get(),
        ]


def set_shm_for_key(bin_key, x, y, prob):
    g = bin_options[bin_key][1]
    g.x.set(x)
    g.y.set(y)
    g.probability.set(prob)
