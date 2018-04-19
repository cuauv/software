import React from "react";
import {Grid, Row, Col, PageHeader} from "react-bootstrap";
import {CUAUV_VEHICLE, CUAUV_LOCALE} from '../framework/environment.jsx';
import {SHMGroupTable, SHMValuesTable} from '../framework/shm-table.jsx';

const depthPressure = [
    {
        key: "Depth",
        shmGroup: "kalman",
        shmVar: "depth",
        round: 3
    },
    {
        key: "Pressure",
        shmGroup: "pressure",
        shmVar: "hull",
        round: 3
    }
];

const power = [
    {
        key: "Total Voltage",
        shmGroup: "merge_status",
        shmVar: "total_voltage",
        round: 3
    },
    {
        key: "Total Current",
        shmGroup: "merge_status",
        shmVar: "total_current",
        round: 3
    }
]

const thrusters = {
    name: "Thrusters",
    shmGroup: "motor_desires"
};

const actuators = {
    name: "Actuators",
    shmGroup: "actuator_desires"
};

export class Index extends React.Component {
    render() {
        return (
            <div>
                <PageHeader>{CUAUV_VEHICLE} @ {CUAUV_LOCALE}</PageHeader>
                <Grid>
                    <Row>
                        <Col md={6}>
                            <SHMValuesTable keyValues={depthPressure} />
                        </Col>
                        <Col md={6}>
                            <SHMValuesTable keyValues={power} />
                        </Col>
                    </Row>
                    <Row>
                        <Col md={6}>
                            <SHMGroupTable name={thrusters.name} shmGroup={thrusters.shmGroup} />
                        </Col>
                        <Col md={6}>
                            <SHMGroupTable name={actuators.name} shmGroup={actuators.shmGroup} />
                        </Col>
                    </Row>
                </Grid>
            </div>
        );
    }
}
