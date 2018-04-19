import React from "react";
import {Row, Col, PageHeader, Button} from "react-bootstrap";
import keydown, { Keys } from 'react-keydown';
import { CUAUV_VEHICLE, CUAUV_LOCALE } from '../framework/environment.jsx';
import { SHMValuesTable } from '../framework/shm-table.jsx';

const { up, down, left, right, space, z } = Keys;

const leftColumn = [
    {
        key: "Heading",
        shmGroup: "kalman",
        shmVar: "heading",
        round: 3
    },
    {
        key: "Velocity X",
        shmGroup: "kalman",
        shmVar: "velx",
        round: 3
    },
    {
        key: "Velocity Y",
        shmGroup: "kalman",
        shmVar: "vely",
        round: 3
    },
]

const rightColumn = [
    {
        key: "Depth",
        shmGroup: "kalman",
        shmVar: "depth",
        round: 3
    },
    {
        key: "Pitch",
        shmGroup: "kalman",
        shmVar: "pitch",
        round: 3
    },
    {
        key: "Roll",
        shmGroup: "kalman",
        shmVar: "roll",
        round: 3
    },
]

class DriveControlHelm extends React.Component {
    render() {
        return (
            <Row>
                <Col md={6}>
                    <SHMValuesTable keyValues={leftColumn} />
                </Col>
                <Col md={6}>
                    <SHMValuesTable keyValues={rightColumn} />
                </Col>
            </Row>
        );
    }
}

class SoftkillButton extends React.Component {
    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (event.which === space) {
            this.onSoftkillButtonClick();
        }
    }

    onSoftkillButtonClick() {
        $.post(window.location.origin + '/admin/kill',
            function(data) {
                console.log(data);
            }
        );
    }

    render() {
        let buttonClickHandler = () => this.onSoftkillButtonClick();
        return (
            <Button class="softkill-button" style={this.props.style} bsStyle="danger" onClick={buttonClickHandler}>
                Soft Kill
            </Button>
        );
    }
}

class ZeroButton extends React.Component {
    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (event.which === z) {
            this.onZeroButtonClick();
        }
    }

    onZeroButtonClick() {
        $.post(window.location.origin + '/drive/zero',
            function(data) {
                console.log(data);
            }
        );
    }

    render() {
        let buttonClickHandler = () => this.onZeroButtonClick();
        return (
            <Button class="drive-button" style={this.props.style} bsStyle="warning" onClick={buttonClickHandler}>
                Zero
            </Button>
        );
    }
}

class MovementButton extends React.Component {
    onMovementButtonClick(speed) {
        let delta = speed * this.delta;
        $.post(window.location.origin + '/drive/movement/' + this.direction + '/' + delta,
            function(data) {
                console.log(data);
            }
        );
    }

    render() {
        let buttonClickHandler = () => this.onMovementButtonClick(1);
        return (
            <Button class="drive-button" style={this.props.style} bsStyle="primary" onClick={buttonClickHandler}>
                {this.name}
            </Button>
        );
    }
}

class VelocityButton extends React.Component {
    onVelocityButtonClick(speed) {
        let delta = speed * this.delta;
        $.post(window.location.origin + '/drive/velocity/' + this.direction + '/' + delta,
            function(data) {
                console.log(data);
            }
        );
    }

    render() {
        let buttonClickHandler = () => this.onVelocityButtonClick(1);
        return (
            <Button class="drive-button" style={this.props.style} bsStyle="primary" onClick={buttonClickHandler}>
                {this.name}
            </Button>
        );
    }
}

class ForwardButton extends VelocityButton {
    constructor(props) {
        super(props);
        this.name = " surge forward";
        this.direction = "x";
        this.delta = 1.0;
        this.mapping = {};
        this.mapping[Keys[1]] = 0.0;
        this.mapping[Keys[2]] = 0.1;
        this.mapping[Keys[3]] = 0.2;
        this.mapping[Keys[4]] = 0.3;
        this.mapping[Keys[5]] = 0.4;
        this.mapping[Keys[6]] = 0.5;
        this.mapping[Keys[7]] = 0.6;
        this.mapping[Keys[8]] = 0.7;
        this.mapping[Keys[9]] = 0.8;
        this.mapping[Keys[0]] = 0.9;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (!event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onVelocityButtonClick(this.mapping[event.which]);
        }
    }
}

class BackButton extends VelocityButton {
    constructor(props) {
        super(props);
        this.name = "surge back";
        this.direction = "x";
        this.delta = -1.0;
        this.mapping = {};
        this.mapping[Keys[1]] = 0.0;
        this.mapping[Keys[2]] = 0.1;
        this.mapping[Keys[3]] = 0.2;
        this.mapping[Keys[4]] = 0.3;
        this.mapping[Keys[5]] = 0.4;
        this.mapping[Keys[6]] = 0.5;
        this.mapping[Keys[7]] = 0.6;
        this.mapping[Keys[8]] = 0.7;
        this.mapping[Keys[9]] = 0.8;
        this.mapping[Keys[0]] = 0.9;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onVelocityButtonClick(this.mapping[event.which]);
        }
    }
}

class SwayLeft extends VelocityButton {
    constructor(props) {
        super(props);
        this.name = "sway left";
        this.direction = "y";
        this.delta = -0.1;
        this.mapping = {};
        this.mapping[left] = 1;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onVelocityButtonClick(this.mapping[event.which]);
        }
    }
}

class SwayRight extends VelocityButton {
    constructor(props) {
        super(props);
        this.name = "sway right";
        this.direction = "y";
        this.delta = 0.1;
        this.mapping = {};
        this.mapping[right] = 1;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onVelocityButtonClick(this.mapping[event.which]);
        }
    }
}

class HeadingLeftButton extends MovementButton {
    constructor(props) {
        super(props);
        this.name = "heading left";
        this.direction = "h"
        this.delta = -5;
        this.mapping = {};
        this.mapping[left] = 1;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (!event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onMovementButtonClick(this.mapping[event.which]);
        }
    }
}

class HeadingRightButton extends MovementButton {
    constructor(props) {
        super(props);
        this.name = "heading right";
        this.direction = "h"
        this.delta = 5;
        this.mapping = {};
        this.mapping[right] = 1;
    }

    componentWillUpdate() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (!event.shiftKey && this.mapping[event.which] !== undefined) {
            this.onMovementButtonClick(this.mapping[event.which]);
        }
    }
}

class DepthUpButton extends MovementButton {
    constructor(props) {
        super(props);
        this.name = "depth up";
        this.direction = "z"
        this.delta = -0.1;
        this.mapping = {};
        this.mapping[up] = 1;
    }

    componentWillReceiveProps() {
        // this is called twice because of @keydown setState, so we ignore duplicate
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (this.mapping[event.which] !== undefined) {
            this.onMovementButtonClick(this.mapping[event.which]);
        }
    }
}

class DepthDownButton extends MovementButton {
    constructor(props) {
        super(props);
        this.name = "depth down";
        this.direction = "z"
        this.delta = 0.1;
        this.mapping = {};
        this.mapping[down] = 1;
    }

    componentWillReceiveProps() {
        if (event === this.prevEvent) {
            return;
        }
        this.prevEvent = event;
        if (this.mapping[event.which] !== undefined) {
            this.onMovementButtonClick(this.mapping[event.which]);
        }
    }
}

class DirectionButtons extends React.Component {
    render() {
        return (
            <div>
                <Row>
                    <Col xs={4} class="text-left">
                        <HeadingLeftButton style={{marginBottom: "10px"}} />
                    </Col>
                    <Col xs={4} class="text-center">
                        <ForwardButton style={{marginBottom: "10px"}} />
                    </Col>
                    <Col xs={4} class="text-right">
                        <HeadingRightButton style={{marginBottom: "10px"}} />
                    </Col>
                </Row>
                <Row>
                    <Col xs={4} class="text-right">
                        <SwayLeft />
                    </Col>
                    <Col xs={4} class="text-center">
                        <ZeroButton />
                    </Col>
                    <Col xs={4} class="text-left">
                        <SwayRight />
                    </Col>
                </Row>
                <Row>
                    <Col xs={4} xsOffset={4} class="text-center">
                        <BackButton style={{marginTop: "10px"}} />
                    </Col>
                </Row>
                <div style={{height: "20px"}}></div>
            </div>
        );
    }
}

class DepthButtons extends React.Component {
    render() {
        return (
            <div>
                <Row>
                    <Col xs={4} xsOffset={4} class="text-center">
                        <DepthUpButton style={{marginBottom: "10px"}} />
                    </Col>
                </Row>

                <Row>
                    <Col xs={4} xsOffset={4} class="text-center">
                        <DepthDownButton />
                    </Col>
                </Row>
            </div>
        );
    }
}

@keydown
export class Drive extends React.Component {
    render() {
        return (
            <div>
                <PageHeader>Drive</PageHeader>
                <DriveControlHelm />
                <Row>
                    <Col sm={6}>
                        <DirectionButtons />
                    </Col>
                    <Col sm={6}>
                        <DepthButtons />
                    </Col>
                </Row>
                <SoftkillButton />
            </div>
        );
    }
}
