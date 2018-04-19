import React from "react";
import {PageHeader, Button, ButtonToolbar, MenuItem, DropdownButton} from "react-bootstrap";
import {CUAUV_VEHICLE, CUAUV_LOCALE} from '../framework/environment.jsx';
import '../../static/js/jqconsole.min.js';

const buttons = [
    {
        name: "Syscheck",
        cmd: "auv-syscheck"
    },
]
const dropButtons = [
    {
        name: "Thruster Test",
        grp_name: "Test all thrusters",
        grp_cmd: "auv-thruster-test",
        cmd: "auv-spin-thruster",
        json_url: "/test/thruster/list"
    },
    {
        name: "Actuator Test",
        grp_name: "Test all actuators",
        grp_cmd: "auv-actuator-test",
        cmd: "auv-fire-actuator",
        json_url: "/test/actuator/list"
    },
]

class Terminal extends React.Component {
    constructor(props) {
        super(props);
        this.jqconsole = null;
    }

    componentDidMount() {
        this.jqconsole = $('#console').jqconsole('', [CUAUV_VEHICLE, '@', CUAUV_LOCALE, '$ '].join(''));
        this.ws = new WebSocket('ws://' + window.location.host + '/test/ws');
        this.ws.onmessage = (m) => this.handleOutput(m);
        this.startPrompt();
    }

    componentWillUnmount() {
        this.ws.close();
    }

    componentDidUpdate() {
        let cmd = this.props.currentPrompt;
        if (cmd !== "") {
            this.jqconsole.SetPromptText(cmd);
            // Abort the current prompt to run this command.
            this.jqconsole.AbortPrompt();
            this.sendData(cmd, "");
        }
    }

    startPrompt() {
        let temp = this;
        this.jqconsole.Prompt(true, function(cmd) {
            if (cmd !== "") {
                temp.sendData(cmd);
            }
            // Restart prompt on empty line
            else {
                temp.startPrompt();
            }
        });
    }

    startInput() {
        let temp = this;
        this.jqconsole.Input(function(data) {
            temp.sendData("", data + '\n');
        });
    }

    sendData(cmd, data) {
        this.ws.send(JSON.stringify({
            type: cmd,
            data: data
        }));
        // Start input after sending data
        this.startInput();
    }

    handleOutput(message) {
        let data = JSON.parse(message.data);
        if (data.data !== "") {
            // Only write output if there was data associated with it
            this.jqconsole.Write(data.data, 'jqconsole-output');
        }
        if (data.type == "ERROR" || data.type == "EOF") {
            this.jqconsole.AbortPrompt();
            // Restart the prompt.
            this.startPrompt();
            this.props.onCmdFinished();
        }
    }

    render() {
        return (
            <div id="console"></div>
        );
    }

}

class TestDropdownButton extends React.Component {
    constructor(props) {
        super(props);
        this.state = {}
    }

    componentWillMount() {
        $.get(window.location.origin + this.props.json_url, (data) => (
            this.setState({
                menuItems: data.msg
            })
        ), 'json');
    }

    onTestButtonClick(cmd) {
        this.props.onTestButtonClick(cmd);
    }

    onMenuItemClick(grp_cmd, cmd) {
        return (eventKey) => {
            if (eventKey === "") {
                this.onTestButtonClick(grp_cmd);
            }
            else {
                this.onTestButtonClick([cmd, eventKey].join(" "));
            }
        }
    }

    renderMenuItems() {
        if (this.state.menuItems === undefined) {
            return null;
        }
        return (
            this.state.menuItems.map(name =>
                <MenuItem key={name} eventKey={name}>{name}</MenuItem>
            )
        );
    }

    render() {
        return (
            <DropdownButton id={this.props.name} key={this.props.name} title={this.props.name} bsStyle="primary" onSelect={this.onMenuItemClick(this.props.grp_cmd, this.props.cmd)} disabled={this.props.disabled}>
                <MenuItem eventKey="">{this.props.grp_name}</MenuItem>
                { this.renderMenuItems() }
            </DropdownButton>
        )
    }
}

class TestButtons extends React.Component {
    onTestButtonClick(cmd) {
        this.props.onTestButtonClick(cmd);
    }

    generateButtons() {
        return buttons.map(button =>
            <Button key={button.name} bsStyle="primary" onClick={() => this.onTestButtonClick(button.cmd)} disabled={this.props.disabled}>{button.name}</Button>
        );
    }

    generateDropdownButtons() {
        return dropButtons.map(button =>
            <TestDropdownButton key={button.name} name={button.name} grp_name={button.grp_name} grp_cmd={button.grp_cmd} cmd={button.cmd} json_url={button.json_url} onTestButtonClick={(cmd) => this.onTestButtonClick(cmd)} disabled={this.props.disabled} />
        );
    }

    render() {
        return (
            <ButtonToolbar class="test-button-wrapper">
                { this.generateButtons() }
                { this.generateDropdownButtons() }
            </ButtonToolbar>
        );
    }
}

export class Test extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            currentPrompt: "",
            disabledButtons: false
        }
    }

    testButtonHandler(cmd) {
        this.setState({
            currentPrompt: cmd,
            disabledButtons: true
        });
    }

    enableButtons() {
        this.setState({
            currentPrompt: "",
            disabledButtons: false
        });
    }

    render() {
        return (
            <div>
                <PageHeader>Testing</PageHeader>
                <TestButtons onTestButtonClick={this.testButtonHandler.bind(this)} disabled={this.state.disabledButtons} />
                <Terminal onCmdFinished={this.enableButtons.bind(this)} currentPrompt={this.state.currentPrompt} />
            </div>
        );
    }
}
