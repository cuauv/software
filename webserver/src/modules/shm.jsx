import React from "react";
import {PageHeader, PanelGroup, Panel, Table} from "react-bootstrap";

class SHMGroupList extends React.Component {
    constructor(props) {
        super(props);
    }

    render() {
        // this is needed because render is called before the parent has all the groups from the websocket
        if (this.props.groups === undefined) {
            return null;
        }
        return (
            <PanelGroup>
                {
                    this.props.groups.map((group, index) => {
                        if (group.indexOf(this.props.query) != -1) {

                            //this.props.found = true;
                            return <SHMGroup name={group} key={index} />;
                        } 

                    })
                    
                }
            </PanelGroup>
        );
    }
}


class SHMGroup extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            expanded: false,
        };
        this.toggleExpand = this.toggleExpand.bind(this);
    }

    toggleExpand() {
        this.setState(previousState => ({
            expanded: !previousState.expanded
        }));
    }

    expandedGroup() {
        if (this.state.expanded) {
            return (
                <SHMGroupContent name={this.props.name} />
            );
        }
    }

    render() {
        return (
            <Panel class="shm-group" header={this.props.name} eventKey={this.props.key} bsStyle="info" collapsible expanded={this.state.expanded} onSelect={this.toggleExpand}>
                <Table striped condensed hover fill>
                    {this.expandedGroup()}
                </Table>
            </Panel>
        );
    }
}

class SHMGroupContent extends React.Component {
    constructor(props) {
        super(props);
        this.state = {};
    }

    componentDidMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/shm/ws');
        this.ws.onopen = () => this.ws.send(JSON.stringify({type: "groupWatch", group: this.props.name}));
        this.ws.onmessage = (m) => this.handleGroupUpdate(m);
    }

    handleGroupUpdate(message) {
        let data = JSON.parse(message.data);
        console.log(data.type);
        this.setState(data.data);
    }

    componentWillUnmount() {
        this.ws.close();
    }

    render() {
        return (
            <tbody>
            {
                Object.keys(this.state).map(key => (
                    <tr key={key}>
                        <td class="key">{key}</td>
                        <td class="value">{this.state[key]}</td>
                    </tr>
                ))
            }
            </tbody>
        )
    }
}

export class SHM extends React.Component {
    constructor(props) {
        super(props);
        this.state = {query: ""};
        this.handleGroupListUpdate = this.handleGroupListUpdate.bind(this);
        this.handleQueryUpdate = this.handleQueryUpdate.bind(this);
    }

    componentWillMount() {
        this.ws = new WebSocket('ws://' + window.location.host + '/shm/ws');
        this.ws.onopen = () => this.ws.send(JSON.stringify({type: "groupList"}));
        this.ws.onmessage = (m) => this.handleGroupListUpdate(m);
    }

    handleGroupListUpdate(message) {
        let data = JSON.parse(message.data);
        console.log(data.type);
        this.setState({groups: data.data});
    }

    handleQueryUpdate(e){
        this.setState({query: e.target.value});
    }

    render() {
        return (
            <div>
                <PageHeader>SHM</PageHeader>
                <div class="search-wrapper" >
                    <input class="form-control" type="text" placeholder="Search" onChange={this.handleQueryUpdate}/>
                </div>
                <SHMGroupList groups={this.state.groups} query={this.state.query} found={false}/>
            </div>
        );
    }
}

