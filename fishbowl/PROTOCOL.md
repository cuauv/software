# CUAUV Simulator Network Protocol Documentation

Last updated June 9, 2015.

The simulator is implemented as a server. The server can have multiple clients
connected simultaneously. All clients connected to a server interact with one
simulated world through the network protocol.

When the simulator is started, the simulation environment is set to a default
environment. The server program accepts arguments that specify properties of the
default environment and of the simulation, such as simulation speed and which
controller to use.

# Networking Layer

The protocol operates over TCP.

# Request-Response Model

One request sent by a client corresponds to one response sent by the server,
with the exception of out-of-band messages. In-band responses are sent in the
same order in which the corresponding requests were sent to the server.

When the server shuts down gracefully, it sends COMPLETED to each connected
client.

# Message Format

Numbers are sent in network byte order (i.e. big endian). We will describe the
format of messages using a C-like notation. For example, the following describes
a message consisting of a byte followed by an 32-bit signed integer, followed by
a double-precision IEEE 754 floating-point number, followed by an 32-bit
unsigned integer length prefix, followed by a variable number of bytes (not a
pointer), with length determined by the preceding length prefix. (There is no
padding for structure alignment as there would be in C.)

```
struct {
	uint8 a;
    int32 b;
    float64 c;
    uint32 d_len;
    byte* d;
};
```

ints are in two's complement.
uints are unsigned ints.
floats are in IEEE 754 format.

bools are represented as uint8s.
A value of 0 is false, all other values are true.

Every message is preceded by a header, consisting of a uint8 (called the "type
ID") whose value identifies the type of the message, and a uint32 (called the
"message length") whose value identifies the length in bytes of the following
message body.

# Items

An item is something with an ID that is recorded by the server. Items include
entities, objects, and cameras. Entities are physical objects, such as the
vehicle. Objects are engines and forces. Cameras are attached to entities and
allow querying for the apparent positions of entities in one-shot as well as
streaming modes.

Why are cameras not objects? Cameras require new message types while objects
should not. In the future, hydrophones will be added, and they will be a whole
new class of item. A future version of the protocol might generalize all of
these.

Also in the future, it is planned that triggers will be associated with bytecode
that is interpreted by the server. The bytecode programs act on entity
properties and can raise TRIGGERED responses. Currently, triggers send
out-of-band messages back to listening clients.

## Item Types

An item type is represented as a uint8.

| ID | Type                   |
|----|------------------------|
| 0  | Entity                 |
| 1  | Object                 |
| 2  | Camera                 |
| 3  | Trigger                |
| 4  | Entity Stream          |
| 5  | Camera Stream          |
| 6  | Camera Broadcast       |
| 7  | Extended Entity Stream |
| 8  | Spy                    |

# Attributes

Attributes are information associated with entities.
Built-in attributes have uint8 IDs listed in the Attribute IDs section.
Extended attributes have string keys and opaque byte arrays as data.
Extended attributes can have keys at most 255 bytes long, and data at most
2^32 - 1 bytes long.

## Attribute IDs

| ID | Attribute   | Type       |
|----|-------------|------------|
| 0  | `x`         | float64[3] |
| 1  | `v`         | float64[3] |
| 2  | `a`         | float64[3] |
| 3  | `q`         | float64[4] |
| 4  | `w`         | float64[3] |
| 5  | `corporeal` | bool       |

# Default Environment

The default simulator environment consists of the following:

- A vehicle entity, with attributes populated with values from the vehicle
  configuration. The vehicle entity ID is 0.
- A gravity force. The gravity force ID is 0. A buoyancy engine attached to the
- vehicle, with magnitude and location from
  the vehicle configuration. The buoyancy engine ID is 0.
- Drag plane engines attached to the vehicle, with properties from the vehicle
  configuration. The drag plane engine IDs are 1 through n, where n is the
  number of drag planes specified in the vehicle configuration.

# Request Types

The ? column indicates the implementation status. Y means that the request type
is fully implemented, N means not yet implemented, and - means a partial
implementation exists.

| ID  | Name     | ? | Short Description                                     |
|-----|----------|---|-------------------------------------------------------|
| 0   | REMOVE   | - | Remove an entity, object, or camera from the world.   |
| 1   | ELIST    | Y | List the IDs of the entities in the world.            |
| 2   | EADD     | Y | Add an entity to the world.                           |
| 3   | ESET     | Y | Set an entity attribute.                              |
| 4   | EGET     | Y | Get an entity attribute.                              |
| 5   | OCLASSES | N | Get a list of the available classes of objects.       |
| 6   | OLIST    | N | List the IDs of the objects in the world.             |
| 7   | OADD     | N | Add an object to the world.                           |
| 8   | OSET     | N | Set an object attribute.                              |
| 9   | CLIST    | N | List the IDS of the cameras in the world.             |
| 10  | CADD     | Y | Add a camera to the world.                            |
| 11  | CSET     | Y | Set a camera attribute.                               |
| 12  | CVIEW    | N | Get the apparent position of an entity from a camera. |
| 13  | TADD     | Y | Add a trigger.                                        |
| 14  | ESTREAM  | Y | Start an entity attribute stream.                     |
| 15  | CSTREAM  | Y | Start an camera view stream.                          |
| 16  | CBCAST   | N | Start a camera broadcast to shared memory.            |
| 17  | CONFIG   | N |                                                       |
| 18  | OGET     | N |                                                       |
| 19  | EXSET    | Y | Set an extended entity attribute.                     |
| 20  | EXGET    | Y | Get an extended entity attribute.                     |
| 21  | EXSTREAM | N | Start an extended entity attribute stream.            |
| 22  | SPY      | - | Watch for \*ADD and REMOVE.                           |
| 33  | REASSURE | Y | For debugging. The server always responds with an OK. |
| 254 | RUN      | Y | Unpause the simulation.                               |
| 255 | PAUSE    | Y | Pause the simulation.                                 |

# Response Types

In-band response types have IDs from 0 to 127. Out-of-band response types have IDs from 128 to 255.

| ID  | Name        | Short Description                                                   |
|-----|-------------|---------------------------------------------------------------------|
| 0   | OK          | A request was completed successfully.                               |
| 1   | ERROR       | A request failed to complete.                                       |
| 2   | ADDED       | A request to add something was successful.                          |
| 3   | ELISTED     |                                                                     |
| 4   | EGOT        | Entity attribute data.                                              |
| 5   | OLISTED     |                                                                     |
| 6   | OGOT        |                                                                     |
| 7   | CGOT        |                                                                     |
| 8   | CONFIGED    |                                                                     |
| 9   | EXGOT       |                                                                     |
| 128 | TRIGGERED   | A trigger (without a more specific response type) was activated.    |
| 129 | EDATA       | Entity attribute stream data.                                       |
| 130 | CDATA       | Camera view stream data.                                            |
| 131 | UNTRIGGERED | A trigger (without a more specific response type) was deactivated.  |
| 132 | PSTART      | An entity entered the start of a path trigger.                      |
| 133 | PFAIL       | An entity exited a path after entering it without reaching the end. |
| 134 | PEND        | An entity reached the end of a path trigger successfully.           |
| 135 | EXDATA      | Extended entity attribute stream data.                              |
| 136 | SPIED       | Something was added or removed.                                     |
| 255 | COMPLETED   | The server is shutting down gracefully.                             |

# Request Details

## 0. REMOVE

```
struct {
    uint8 type;
	uint32 id;
};
```

See also Item Types.

## 1. ELIST

```
struct {};
```

## 2. EADD

```
struct xattr {
    uint8 key_len;
    uint8* key;
    uint32 data_len;
    uint32* data;
};

struct {
    float64 m;
    float64 r;

    float64 Ix;
    float64 Iy;
    float64 Iz;

    float64 btom_rqw;
    float64 btom_rqx;
    float64 btom_rqy;
    float64 btom_rqz;

    float64 x;
    float64 y;
    float64 z;

    float64 qw;
    float64 qx;
    float64 qy;
    float64 qz;
    
    bool corporeal;

    uint8 xattrs_n;
    xattr* xattrs;
};
```

ADDED with the entity ID.
If an entity is not `corporeal`, it is not affected by forces or engines. It is
still affected by its `v`.

## 3. ESET

```
struct {
    uint32 id;
    uint8 attribute;
    byte* data;
};
```

x, v, and a are in the world frame. w is in the model frame.

ERROR 0 if there is no entity with the given ID. ERROR 1 if the attribute ID is
invalid.

See also Attribute IDs.

## 4. EGET

```
struct {
    uint32 id;
    uint8 attribute;
    byte* data;
};
```

See also Attribute IDs.

## 7. OADD

```
struct {
    uint32 class_len;
    byte* class;
    uint32* data;
}
```

### turbulence

```
struct {
    float64 fxa;
    float64 fya;
    float64 fza;
    
    float64 fxb;
    float64 fyb;
    float64 fzb;
    
    float64 fda;
    float64 fdb;
    
    float64 txa;
    float64 tya;
    float64 tza;

    float64 txb;
    float64 tyb;
    float64 tzb;
    
    float64 tda;
    float64 tdb;
};
```

## 10. CADD

```
struct {
    uint32 entity_id;

    float64 qw;
    float64 qx;
    float64 qy;
    float64 qz;

    float64 x;
    float64 y;
    float64 z;

    float64 f;
    
    bool enabled;
};
```

Response is an ADDED.

If `enabled` is false, camera streams will not stream and the camera will not
update.

## 11. CSET

```
struct {
    uint32 id;
    uint8 attribute;
    byte* data;
};
```

| ID | Attribute | Type       |
|----|-----------|------------|
| 0  | `q`       | float64[4] |
| 1  | `x`       | float64[3] |
| 2  | `f`       | float64    |
| 3  | `enabled` | uint8      |

## 13. TADD

```
struct collision_config {
    uint32 a;
    uint32 b;
    double r;
};

struct collision_sweep_config {
    uint32 a;
    uint32 b;
};

struct program {
    uint32 n;
    uint8* code;
};

struct path {
    uint32 entity_id;

    double x0;
    double y0;
    double z0;

    double x1;
    double y1;
    double z1;

    double r;
};

struct {
    uint8 type;
    uint8* config;
};
```

| ID | Type              | Configuration            |
|----|-------------------|--------------------------|
| 0  | Collision         | `collision_config`       |
| 1  | Collision (Sweep) | `collision_sweep_config` |
| 2  | Bytecode          | `program`                |
| 3  | Path              | `path`                   |
 
If `r` is negative, it is set to the sum of the radii of the two objects.

NB: Collision currently simply compares the positions of the two entities after
each step, so high relative velocities and low simulation frequencies can cause
collision detection failures.

Sweep collision does a sweep test, but is slightly more expensive.

After a collision and sweep collision trigger that has triggered is no longer in
a triggering state, an UNTRIGGERED message is sent.

Path triggers specify a line in the world. When the entity comes within distance
`r` of `(x0, y0, z0)`, a PSTART message will be sent. If the distance between
the entity and the line subsequently becomes greater than `r`, a PFAIL message
is sent. Hoewver, if the entity first comes within distance `r` of `(x1, y1,
z1)`, then a PEND message is sent, and the path can be restarted.

ADDED

## 14. ESTREAM

```
struct {
    uint32 id;
    uint32 period;
    uint8 n;
    uint8* attributes;
};
```

## 15. CSTREAM

```
struct {
    uint32 id;
    uint32 period;
    uint32 target_id;
};
```

`id` is the ID of the camera. `period` determines how often the CDATA response
is transmitted. A `period` of one corresponds to every timestep, a `period` of
two to every other time step, etc. `target_id` is the ID of the entity to
track.

The response is an ADDED with the ID of the new camera stream.

## 16. CBSTART

```
struct {
    uint32 id;
};
```

Response is an ADDED with the broadcast ID.

## 17. CONFIG

```
struct {};
```

## 19. EXSET

```
struct {
    uint32 id;
    uint8 key_len;
    byte* key;
    uint32 data_len;
    byte* data;
};
```

## 20. EXGET

```
struct {
    uint32 id;
    uint8 key_len;
    byte* key;
};
```

## 21. EXSTREAM

```
struct xattr {
    uint8 key_len;
    byte* key;
};

struct {
    uint32 id;
    uint32 period;
    uint8 n;
    xattr* attributes;
};
```

## 22. SPY

```
struct {};
```

SPY causes subsequent item additions & removals to be sent to this client as
SPIED messages.
This can be stopped using the REMOVE request.

Currently only entity additions & removal notifications are implemented.

## 254. RUN

```
struct {
    uint64 steps;
};
```

If `steps` is 0, run indefinitely. Otherwise, COMPLETED is sent when done.

## 255. PAUSE

```
struct {};
```

# Response Details

## 0. OK

```
struct {};
```

## 1. ERROR

```
struct {
	uint32 code;
    uint32 message_len;
    byte* message;
};
```

## 2. ADDED

```
struct {
    uint8 type;
	uint32 id;
};
```

## 3. ELISTED

```
struct {
    uint32 n;
    uint32* ids;
};
```

## 4. EGOT

```
struct {
    uint8 attribute;
    byte* data;
};
```

## 8. CONFIGED

```
struct {
    float64 frequency;
    float64 speed;
    bool non_real_time;
    bool use_des_thrusters;
};
```

## 9. EXGOT

```
struct {
    uint8 key_len;
    byte* key;
    uint32 data_len;
    byte* data;
};
```

## 128. TRIGGERED

```
struct {
    uint32 id;
};
```

## 129. EDATA

```
struct datum {
    uint8 attribute;
    byte* data;
};

struct {
    uint32 id;
    uint8 n;
    datum* data;
};
```

The attributes are sent in the order they were specified in the ESTREAM request.

## 130. CDATA

```
typedef struct {
    uint32 id;
    float32 x;
    float32 y;
    float32 r;
    float32 d;
};
```

`id` is the stream ID. `r` is the apparent radius of the target entity. `r` is
set to 0 if the target entity would not be visible (e.g. it is coincident with
the camera or behind the camera). `d` is the distance from the camera to the
target entity.

(0, 0) is directly in front of the camera, on its x-axis. Values of x in the
screen plane increase to the right, and values of y increase downwards.

## 131. UNTRIGGERED

```
struct {
    uint32 id;
};
```

## 135. EXDATA

```
struct datum {
    uint8 key_len;
    byte* key;
    uint32 data_len;
    byte* data;
};

struct {
    uint32 id;
    uint8 n;
    datum* data;
};
```

## 136. SPIED

```
struct {
    uint8 event;
    uint8 type;
    uint8 id;
};
```

`event` is 0 if an item was removed and 1 if an item was added.
`type` is the item's type, and `id` is the item's ID.

## 255. COMPLETED

```
typedef struct {};w
```

// vim: set tw=80:
