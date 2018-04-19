import React from 'react'
import {render} from 'react-dom'
import {Router, Route, browserHistory, Link, IndexRoute} from 'react-router'
import {socket} from "./framework/socket.jsx";
import {SHM} from './modules/shm.jsx'
import {Thrusters} from './modules/thrusters.jsx'
import {Test} from './modules/test.jsx'

const components = [
    {
        name: "SHM",
        path: "shm",
        component: SHM
    },
    {
        name: "Thrusters",
        path: "thrusters",
        component: Thrusters
    },
    {
        name: "Test",
        path: "test",
        component: Test
    }
];

function navigationLinks() {
    return components.map(component => (
        <li key={component.path}><Link to={`/${component.path}`} activeClassName="active">{component.name}</Link></li>
    ));
}

class Layout extends React.Component {
    render() {
        return (
            <div>
                <div class="top-bar">
                    <div class="row">
                        <div class="small-12 columns">
                            <div class="top-bar-left">
                                <ul class="menu">
                                    <li class="menu-text"><Link to="/" activeClassName="active">M.E.O.W.</Link></li>
                                    {navigationLinks()}
                                </ul>
                            </div>
                        </div>
                    </div>
                </div>

                <div class="row">
                    <div class="small-12 columns">
                        {this.props.children}
                    </div>
                </div>
            </div>
        )
    }
}

class Links extends React.Component {
    render() {
        return (
            <ul>
                <li><Link to="/" activeClassName="active">Home</Link></li>
                {navigationLinks()}
            </ul>
        )
    }
}


var r = (
    <Router history={browserHistory}>
        <Route name="app" path="/" component={Layout}>
            <IndexRoute component={Links}/>
            {components.map(component =>
                <Route name={component.name} path={component.path} component={component.component}/>)}
        </Route>
    </Router>
);

render(r, document.getElementById('app'));
