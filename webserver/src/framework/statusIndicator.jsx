import React from 'react';

export class StatusIndicator extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            color: "gray"
        };
    }

    componentDidMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/status');
        this.ws.onopen = () => this.ws.send(JSON.stringify({get_status: ["soft_kill", "hard_kill"]}));
        this.ws.onmessage = (m) => this.handleStatusUpdate(m);
        this.ws.onclose = () => this.setState({color: "gray"});
    }

    handleStatusUpdate(message) {
        let data = JSON.parse(message.data);
        console.log(data);
        if (data.hard_kill === 1) {
            this.setState({color: "red"});
        }
        else if (data.soft_kill === 1) {
            this.setState({color: "yellow"});
        }
        else {
            this.setState({color: "green"});
        }
    }

    render() {
        return (
            <span id="navbar-status-indicator" style={{backgroundColor: this.state.color}}></span>
        );
    }
}
