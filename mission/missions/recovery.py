from mission.framework.combinators import Sequential, Conditional
from mission.framework.primitive import Succeed, Fail, Log, FunctionTask
from mission.framework.timing import Timeout, Timed, Timer

from mission.missions.vampire import GrabVampire, Search, CenterAny, which_visible, ReleaseVampire, SearchAnyVampire
from mission.missions.attilus_garbage import MoveNE, SetMarker, UnsetMarker, GoToMarker, PositionMarkers
from mission.missions.crucifix import GrabCrucifix, SearchCrucifix, CenterCrucifix

STATES = ['first', 'second']
crucifix_state = 0

EDGE_RATIO = 0.1

def change_state(state):
    global crucifix_state
    crucifix_state = state
    print(crucifix_state)
    return True

def get_crucifix_found():
    global crucifix_state
    print(crucifix_state)
    return bool(crucifix_state)

def non_crucifix_task():
    """Do non crucifix task first. If crucifix not found, do marker 2"""
    print('why')
    if crucifix_state == 2:
        return STATES[0]
    return STATES[1]

def crucifix_task():
    """Do crucifix task second. If crucifix not found, do marker 1"""
    print('why2')
    if crucifix_state == 2:
        return STATES[1]
    return STATES[0]

Recovery = lambda: Sequential(
    Succeed(Timeout(CenterAny(), 20)),
    SetMarker('first'),
    Conditional(Timeout(SearchCrucifix(), 30), on_success=Succeed(Sequential(CenterCrucifix(), SetMarker('crucifix'), FunctionTask(lambda: change_state(1)))), on_fail=Succeed()),
    FunctionTask(reflect),
    GoToMarker('second'),
    Timeout(SearchAnyVampire(), 40),  # TODO: MAKE THIS WORK
    Succeed(Timeout(CenterAny(), 40)),
    SetMarker('second'),
    Conditional(FunctionTask(get_crucifix_found), on_success=Log('Skipping searching crucifix because found already'), on_fail=Conditional(Timeout(SearchCrucifix(), 30), on_success=Sequential(SetMarker('crucifix'), FunctionTask(lambda: change_state(2))), on_fail=Succeed())),
    GoToMarker(non_crucifix_task),
    # GrabVampire(),
    ReleaseVampire(lambda: edge(non_crucifix_task())),
    GoToMarker(crucifix_task),
    # GrabVampire(),
    ReleaseVampire(lambda: edge(crucifix_task())),
    Conditional(FunctionTask(get_crucifix_found), on_success=\
            Sequential(
                GoToMarker('crucifix'),
                GrabCrucifix(),
                GoToMarker(crucifix_task),
                )
            )
)

Mark = lambda: SetMarker('center')

Test = lambda: Sequential(
        Mark(),
        Timer(20),
        Recovery())

def reflect():
    markers = PositionMarkers()
    point1 = markers.get('center')
    point2 = markers.get('first')
    if point1 is not None and point2 is not None:
        x = point1[0] - (point2[0] - point1[0])
        y = point1[1] - (point2[1] - point1[1])
        markers.set('second', (x, y))

def edge(ed):
    markers = PositionMarkers()
    center = markers.get('center')
    vampire = markers.get(ed)
    return ((vampire[0] - center[0]) * EDGE_RATIO + vampire[0], (vampire[1] - center[1]) * EDGE_RATIO + vampire[1])
