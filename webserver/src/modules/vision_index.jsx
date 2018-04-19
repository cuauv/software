import React from "react";
import {PageHeader} from "react-bootstrap";

export class VisionIndex extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
    }

    componentDidMount() {
        $.get(window.location.pathname + '/modules/active', (data) => (
            this.setState({
                activeModules: data
            })
        ), 'json');
    }

    render() {
        return (
            <div>
                <PageHeader>Vision Modules</PageHeader>
                <ul id="modules">
                    {this.state["activeModules"] == null || this.state["activeModules"].length === 0
                            ? <span>No modules are running!</span>
                            : this.state["activeModules"].map(module => <li key={`${module}`}><a href={`/vision/${module}`}>{module}</a></li>)
                    }
                </ul>
            </div>
        );
    }
}
