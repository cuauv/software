import React from 'react'
import { Nav, Navbar, NavItem } from 'react-bootstrap'
import { LinkContainer } from 'react-router-bootstrap'
import { StatusIndicator } from './framework/statusIndicator.jsx'
import { CUAUV_VEHICLE, CUAUV_LOCALE} from './framework/environment.jsx'
import CUAUV_LOGO_INVERSE from '../static/images/cuauv-inverse.svg'

class HeaderLinks extends React.Component {
    constructor(props) {
        super(props);
    }

    generateLinks() {
        return this.props.links.map(link => (
            // we want "exact" matching for the url === location.pathname
            <LinkContainer exact key={link.name} to={link.path} activeClassName="active">
                <NavItem eventKey={link.name}>{link.name}</NavItem>
            </LinkContainer>
        ));
    }

    render() {
        return (
            <Nav>
                {this.generateLinks()}
            </Nav>
        );

    }
}

export class Header extends React.Component {
    constructor(props) {
        super(props);
        console.log(this.props);
        this.host = window.location.host;
    }

    render() {
        return (
            <Navbar inverse collapseOnSelect>
                <Navbar.Header>
                    <Navbar.Brand>
                        <a class="navbar-brand" href="/">
                            <div class="navbar-logo" dangerouslySetInnerHTML={{__html: CUAUV_LOGO_INVERSE}} />
                            <StatusIndicator />
                        </a>
                    </Navbar.Brand>
                    <Navbar.Toggle />
                </Navbar.Header>
                <Navbar.Collapse>
                    <HeaderLinks links={this.props.links} />
                    <Navbar.Text pullRight>
                        {CUAUV_VEHICLE} @ {CUAUV_LOCALE}
                    </Navbar.Text>
                </Navbar.Collapse>
            </Navbar>
        );
    }
}
