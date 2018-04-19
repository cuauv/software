// TODO: Add prop-types
import React from "react";

function formatId(name) {
    return name.replace(/\ /g, "_");
}

function resizeGrid() {
    // Note: We use manual control, rather than allowing Bootstrap's media
    // queries to dynamically resize the grid, so that we have better control at
    // high resolutions
    const width = $(window).width();
    $('.image-container').each(function() {
        const container = $(this);
        const currentClass = container.attr("class").match(/col[^ ]*/)[0];
        container.removeClass(currentClass);
        if (width > 4000) {
            container.addClass("col-xs-2");
        }
        else if (width > 2000) {
            container.addClass("col-xs-3");
        }
        else if (width > 1500) {
            container.addClass("col-xs-4");
        }
        else {
            container.addClass("col-xs-6");
        }
    });
}

function togglePreprocessorItems() {
    const preprocessorItems = $("li.list-group-item").filter(function(k, v) {
        return v.innerHTML.indexOf("PPX_") !== -1;
    });
    preprocessorItems.each(function(k, v) {
        v.style.backgroundColor = "#cccccc";
    });
    const show = $('#preprocessor-toggle').prop('checked');
    if (show) {
        preprocessorItems.slideDown();
    }
    else {
        preprocessorItems.slideUp();
    }
}

class ImageContainer extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            image: this.props.image,
            data: this.props.image.image,
        };
        this.imageName = this.props.image.image_name;
        this.imageId = formatId(this.imageName);
    }

    componentDidMount() {
        window.requestAnimationFrame(resizeGrid);
        window.requestAnimationFrame(togglePreprocessorItems);
    }

    shouldComponentUpdate(nextProps, nextState) {
        return this.state.data !== nextState.data;
    }

    componentWillReceiveProps(nextProps) {
        this.setState({data: nextProps.image.image});
    }

    render() {
        return (
            <li className="image-container list-group-item col-xs-6">
                <img id={this.imageId} src={'data:image/jpeg;base64,' + this.state.data} className="posted"/>
                <br/>
                {this.imageName}
            </li>
        );
    }
}

class OptionItem extends React.Component {
    constructor(props) {
        super(props);
        this.state = {option: this.props.option};
        this.onChange = this.props.onChange;
        this.type = this.state.option.type;
        this.valueName = formatId(this.state.option.option_name);
        this.sliderName = formatId(this.state.option.option_name) + '_slider';
        this.valueId = '#' + this.valueName;
        this.sliderId = '#' + this.sliderName;
        this.handleOptionUpdate = this.handleOptionUpdate.bind(this);
    }

    shouldComponentUpdate(nextProps, nextState) {
        return this.state.option.value !== nextState.option.value;
    }

    componentWillReceiveProps(nextProps) {
        this.setState({option: nextProps.option});
    }

    propagateUpdate(value) {
        this.props.onChange(this.state.option, value);
    }

    handleOptionUpdate(evt) {
        if (this.type === 'int' || this.type === 'double') {
            // Update slider when value is changed
            if ('min_value' in this.state.option && 'max_value' in this.state.option) {
                $(this.sliderId).slider('value', evt.target.value);
            }
        }
        let value = evt.target.value;
        if (this.type === 'int') {
            value = parseInt(value);
            if (isNaN(value)) {
                value = 0;
            }
        }
        else if (this.type === 'double') {
            value = parseFloat(value);
            if (isNaN(value)) {
                value = 0;
            }
        }
        else if (this.type === 'bool') {
            value = evt.target.checked;
        }
        // Propagate update event to parent component
        this.propagateUpdate(value);
    }

    componentDidMount() {
        if (this.type === 'int' || this.type === 'double') {
            // Initialize JQuery-UI slider
            if ('min_value' in this.state.option && 'max_value' in this.state.option) {
                $(this.sliderId).slider({
                    min: this.state.option.min_value,
                    max: this.state.option.max_value,
                    value: this.state.option.value,
                    slide: function(event, ui) {
                        $(this.valueId).val(ui.value);
                        this.propagateUpdate(ui.value);
                    }.bind(this)
                });
                if (this.type === 'double') {
                    $(this.sliderId).slider("option", "step", 0.001);
                }
            }
        }
        window.requestAnimationFrame(togglePreprocessorItems);
    }

    render() {
        if (this.type === 'int' || this.type === 'double') {
            // Display input box and slider for number option
            return (
                <li className="list-group-item col-xs-12">
                    {this.state.option.option_name + ': '}
                    <input type="text" className="slider_value" id={this.valueName} value={this.state.option.value} onChange={this.handleOptionUpdate}/>
                    <br/>
                    <div id={this.sliderName}></div>
                </li>
            );
        }
        else if (this.type === 'bool') {
            // Display checkbox for boolean option
            return (
                <li className="list-group-item col-xs-12">
                    <input type="checkbox" id={this.valueName} onChange={this.handleOptionUpdate} checked={this.state.option.value}/>
                    {this.state.option.option_name}
                </li>
            );
        }
        else if (this.type === 'str') {
            // Display input box for string option
            return (
                <li className="list-group-item col-xs-12">
                    {this.state.option.option_name + ': '}
                    <input type="text" id={this.valueName} className="text_input" value={this.state.option.value} onChange={this.handleOptionUpdate}/>
                </li>
            );
        }
    }
}

export class VisionModule extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            images: {},
            options: {},
        };
        this.socket = null;
        this.handleOptionUpdate = this.handleOptionUpdate.bind(this);
        this.clearImages = this.clearImages.bind(this);
    }

    getOrderedImages() {
        let orderedImages = [];
        for (let imgName in this.state.images) {
            orderedImages.push(this.state.images[imgName]);
        }
        orderedImages.sort((a, b) => a.image_index - b.image_index);
        return orderedImages;
    }

    getOrderedOptions() {
        let orderedOptions = [];
        for (let optionName in this.state.options) {
            orderedOptions.push(this.state.options[optionName]);
        }
        orderedOptions.sort((a, b) => a.option_index - b.option_index);
        return orderedOptions;
    }

    clearImages() {
        this.setState({images: {}});
    }

    handleOptionUpdate(option, value) {
        this.socket.send(JSON.stringify({
            module: window.MODULE_NAME,
            option: option.option_name,
            value: value
        }));
    }

    componentDidMount() {
        // Change container to full-width
        $('#body').parent().removeClass('container').addClass('container-fluid');
        // Construct path to websocket handler
        let webSocketPath = location.pathname.split('/');
        webSocketPath.splice(2, 0, 'ws');
        webSocketPath = webSocketPath.join('/');
        this.socket = new WebSocket('ws://' + document.domain + ':' + location.port + webSocketPath);
        this.socket.onmessage = function(evt) {
            const msg = JSON.parse(evt.data);
            //console.log("Received", msg);
            if ("image_name" in msg) {
                this.setState({images: Object.assign({}, this.state.images, {[msg.image_name]: msg})});
            }
            else if ("option_name" in msg) {
                if (msg.type === 'str') {
                    msg.value = String.fromCharCode.apply(null, new Uint8Array(msg.value));
                }
                this.setState({options: Object.assign({}, this.state.options, {[msg.option_name]: msg})});
            }
        }.bind(this);
        // Dynamically resize grid layout when window is resized
        $(window).resize(resizeGrid);
    }

    componentWillUnmount() {
        this.socket.close();
    }

    render() {
        return (
            <div id="body" class="container-fluid" role="main">
                {/* <span>{JSON.stringify(this.state)}</span> */}
                <input
                    type="checkbox"
                    id="preprocessor-toggle"
                    onChange={togglePreprocessorItems}
                />
                <label for="preprocessor-toggle">Toggle Preprocessor Options</label>
                <button id="clear-images" onClick={this.clearImages}>Clear Images</button>
                <div class="row">
                    <div class="col-xs-10">
                    <ul class="list-group row" id="images">
                        {this.getOrderedImages().map(img =>
                            <ImageContainer
                                image={img}
                                key={img.image_name}/>
                        )}
                    </ul>
                    </div>
                    <div class="col-xs-2">
                    <ul class="list-group row" id="options">
                        {this.getOrderedOptions().map(option =>
                            <OptionItem
                                option={option}
                                onChange={this.handleOptionUpdate}
                                key={option.option_name}/>
                        )}
                    </ul>
                    </div>
                </div>
            </div>
        );
    }
}
