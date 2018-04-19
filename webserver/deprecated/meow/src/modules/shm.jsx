import React from "react";
import {socket} from "../framework/socket.jsx";


const GroupList = React.createClass({
    render() {
        return (
            <div className="moduleList">
                <h2>Groups:</h2>
                <div className="row">
                    {
                        this.props.groups.map((group, i) => {
                            return (
                                <Group
                                    key={i}
                                    name={group}
                                />
                            );
                        })
                    }
                </div>
            </div>
        );
    }
});


const Group = React.createClass({
    getInitialState() {
        return {expanded: false};
    },

    toggleExpand() {
        this.setState(previousState => previousState.expanded = !previousState.expanded);
    },

    expandedGroup() {
        if (this.state.expanded) {
            return (
                <GroupContent name={this.props.name}/>
            )
        }
    },

    render()
    {
        return (
            <div className="small-12 columns group" onClick={this.toggleExpand}>
                <span>{this.props.name}</span>
                {this.expandedGroup()}
            </div>
        );
    }
});

const GroupContent = React.createClass({
    getInitialState() {
        return {};
    },

    handleGroupUpdate(data) {
        this.setState(data);
    },

    componentWillMount() {
        console.log(`Subscribing to group ${this.props.name}`);
        socket.emit("join", {group: this.props.name});
        socket.on(`groupUpdate-${this.props.name}`, this.handleGroupUpdate)
    },

    componentWillUnmount() {
        console.log(`Unsubscribing to group ${this.props.name}`);
        socket.emit("leave", {group: this.props.name});
        socket.off(`groupUpdate-${this.props.name}`, this.handleGroupUpdate)
    },

    render() {
        return (
            <div>
                {
                    Object.keys(this.state).map(key =>(
                            <div className="row variable" key={key}>
                                <div className="small-6 columns">{key}</div>
                                <div className="small-6 columns">{this.state[key]}</div>
                            </div>
                        )
                    )
                }
            </div>
        )
    }
});


export const SHM = React.createClass({
    getInitialState() {
        return {groups: []};
    },

    componentDidMount() {
        socket.emit("listGroups", {}, this._initialize);
    },

    _initialize(groups) {
        this.setState({groups});
    },

    render() {
        return (
            <div>
                <h3>SHM: </h3>
                <GroupList
                    groups={this.state.groups}
                />
            </div>
        );
    }
});

