import React from "react";
import {Grid, Row, Col, PageHeader, Button} from "react-bootstrap";
import keydown, { Keys } from 'react-keydown';
import {CUAUV_VEHICLE, CUAUV_LOCALE} from '../framework/environment.jsx';

const { space } = Keys;

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
            },
        'json');
    }

    render() {
        let buttonClickHandler = () => this.onSoftkillButtonClick();
        return (
            <Button class="softkill-button" style={this.props.style} bsStyle="danger" onClick={buttonClickHandler}>
                SoftKill
            </Button>
        );
    }
}

class UnSoftkillButton extends React.Component {
    // no keyboard shortcut

    onUnSoftkillButtonClick() {
        let yes = confirm("Please confirm that you want to Un-SoftKill.");
        if (yes) {
            $.post(window.location.origin + '/admin/kill', JSON.stringify({ kill: 0 }),
                function(data) {
                    console.log(data);
                },
            'json');
        }
    }

    render() {
        let buttonClickHandler = () => this.onUnSoftkillButtonClick();
        return (
            <Button class="drive-button" style={this.props.style} bsStyle="success" onClick={buttonClickHandler}>
                Un-SoftKill
            </Button>
        );
    }
}

@keydown
export class Admin extends React.Component {
    render() {
        return (
            <div>
                <PageHeader>Administration</PageHeader>
                <Grid>
                    <Row>
                        <Col md={6} mdOffset={3}>
                            <UnSoftkillButton />
                        </Col>
                    </Row>
                    <Row>
                        <Col md={6} mdOffset={3}>
                            <SoftkillButton />
                        </Col>
                    </Row>
                </Grid>
            </div>
        );
    }
}
