import React from "react";
import {socket} from "../framework/socket.jsx";

const group_name = "motor_desires";

export const Thrusters = React.createClass({
    getInitialState(){
        return {
            aft_port: "No data.",
            sway_aft: "No data.",
            fore_port: "No data.",
            fore_starboard: "No data.",
            sway_fore: "No data.",
            aft_starboard: "No data.",
            port: "No data.",
            starboard: "No data."
        }
    },

    handleThrustersUpdate(thrusters){
        this.setState(thrusters);
    },

    componentWillMount() {
        console.log(`Subscribing to group ${group_name}`);
        socket.emit("join", {group: group_name});
        socket.on(`groupUpdate-${group_name}`, this.handleThrustersUpdate)
    },

    componentWillUnmount() {
        console.log(`Unsubscribing to group ${group_name}`);
        socket.emit("leave", {group: group_name});
        socket.off(`groupUpdate-${group_name}`, this.handleThrustersUpdate)
    },

    render() {
        return (
            <div>
                <h1>Thrusters Status</h1>
                <div style={{position: "relative"}}>
                    <img src="/static/topview-thor.png" style={{"max-width": "none"}}/>
                    <span style={{position: "absolute", top: "85px", left: "298px"}}>fore_starboard: {this.state.fore_starboard}</span>
                    <span style={{position: "absolute", top: "0px", left: "575px"}}>starboard: {this.state.starboard}</span>
                    <span style={{position: "absolute", top: "90px", left: "660px"}}>aft_starboard: {this.state.aft_starboard}</span>
                    <span style={{position: "absolute", top: "261px", left: "805px"}}>sway_aft: {this.state.sway_aft}</span>
                    <span style={{position: "absolute", top: "425px", left: "660px"}}>aft_port: {this.state.aft_port}</span>
                    <span style={{position: "absolute", top: "520px", left: "595px"}}>port: {this.state.port}</span>
                    <span style={{position: "absolute", top: "433px", left: "280px"}}>fore_port: {this.state.fore_port}</span>
                    <span style={{position: "absolute", top: "255px", left: "60px"}}>sway_fore: {this.state.sway_fore}</span>
                </div>
                <h2>Raw data:</h2>

                <ul>
                    <li>fore_starboard: {this.state.fore_starboard}</li>
                    <li>starboard: {this.state.starboard}</li>
                    <li>aft_starboard: {this.state.aft_starboard}</li>
                    <li>sway_aft: {this.state.sway_aft}</li>
                    <li>aft_port: {this.state.aft_port}</li>
                    <li>port: {this.state.port}</li>
                    <li>fore_port: {this.state.fore_port}</li>
                    <li>sway_fore: {this.state.sway_fore}</li>
                </ul>
            </div>
        )
    }
});
