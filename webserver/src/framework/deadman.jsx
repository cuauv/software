import React from 'react';
import {Button, Modal} from 'react-bootstrap';

export class Deadman extends React.Component {
    /* checks every second that the webserver is online */
    constructor(props) {
        super(props);
        this.state = {
            showModal: false,
        };
    }

    componentDidMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/deadman');
        this.ws.onopen = () => this.onopen();
        this.ws.onclose = () => this.onclose();
        this.ws.onmessage = (m) => this.handlePing(m);
    }

    shouldComponentUpdate(nextProps, nextState) {
        return this.state.showModal !== nextState.showModal;
    }

    onopen() {
        let current_ping = Date.now() / 1000;
        this.setState({
            showModal: false,
            last_ping: current_ping,
            ping_check: setInterval(this.check.bind(this), 1000),
        });
        this.ws.send(JSON.stringify({ping: current_ping}));
    }

    handlePing(message) {
        let data = JSON.parse(message.data);
        //console.log(data);
        this.setState({last_ping: data.ping})
    }

    onclose() {
        clearInterval(this.state.ping_check);
        this.setState({showModal: true});
    }

    check() {
        let current_ping = Date.now() / 1000;
        if (current_ping - this.state.last_ping > 3) {
            this.ws.close();
        }
        else {
            this.ws.send(JSON.stringify({ping: Date.now()}));
        }
    }

    render() {
        let buttonClose = () => this.setState({showModal: false});
        return (
            <Modal show={this.state.showModal} onHide={buttonClose}>
                <Modal.Header closeButton>
                    <Modal.Title>Warning: Disconnected</Modal.Title>
                </Modal.Header>
                <Modal.Body>
                    <p>You have been disconnected from the sub. Refresh to try again.</p>
                </Modal.Body>
                <Modal.Footer>
                    <Button onClick={buttonClose}>Close</Button>
                </Modal.Footer>
            </Modal>
        );
    }
}
