import React from "react";
import {PanelGroup, Panel, Table} from "react-bootstrap";

export class SHMGroupTable extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
    }

    componentDidMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/shm/ws');
        this.ws.onopen = () => this.ws.send(JSON.stringify({type: "groupWatch", group: this.props.shmGroup}));
        this.ws.onmessage = (m) => this.handleGroupUpdate(m);
    }

    handleGroupUpdate(message) {
        let data = JSON.parse(message.data);
        this.setState(data.data);
    }

    componentWillUnmount() {
        this.ws.close();
    }

    render() {
        return (
            <Table class="shm-group" striped bordered condensed>
                <thead>
                    <tr>
                        <th class="key">{this.props.name}</th>
                        <th class="value">Status</th>
                    </tr>
                </thead>
                <tbody>
                    {
                        Object.keys(this.state).sort().map(key => (
                            <tr key={key}>
                                <td class="key">{key}</td>
                                <td class="value">{this.state[key]}</td>
                            </tr>
                        ))
                    }
                </tbody>
            </Table>
        );
    }
}

export class SHMValuesTable extends React.Component {
    render() {
        return (
            <Table class="shm-group" striped bordered condensed>
                <tbody>
                    {
                        this.props.keyValues.map(row => (
                            <SHMValueRow key={row.key} name={row.key} shmGroup={row.shmGroup} shmVar={row.shmVar} round={row.round} />
                        ))
                    }
                </tbody>
            </Table>
        );
    }
}

class SHMValueRow extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
    }

    componentDidMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/shm/ws');
        this.ws.onopen = () => this.ws.send(JSON.stringify({type: "groupWatch", group: this.props.shmGroup}));
        this.ws.onmessage = (m) => this.handleGroupUpdate(m);
    }

    handleGroupUpdate(message) {
        let data = JSON.parse(message.data);
        this.setState(data.data);
    }

    componentWillUnmount() {
        this.ws.close();
    }

    render() {
        let value = this.state[this.props.shmVar];
        if (this.props.round !== undefined) {
            value = parseFloat(value).toFixed(this.props.round);
        }
        return (
            <tr key={this.props.name}>
                <td class="key">{this.props.name}</td>
                <td class="value">{value}</td>
            </tr>
        );
    }
}

