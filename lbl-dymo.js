#!/usr/bin/node
var pngparse = require('pngparse');

function reduceRow( imageRowBytes ) {
	// reduce 672 bytes of image data to 84 bytes of bit-pixels
	var row=new Buffer(84);
	for (var i=0; i<672; i+=8) {
		// examine 8 bytes at a time of the image data
		var bytes = imageRowBytes.slice( i, i+8 );
		// compose one byte with a bit for each input byte
		var b = bytes.reduce( 
			// any bytes with a value less than FF (white) are set to a bit of 1
			function( sum,v,i,a ) { return sum + ( v<255 ? Math.pow(2,a.length-i-1) : 0 ) }, 
			0
		)
		row[i/8]=b;
	}
	return row;
}

function main( err, img ) {
	if (err) {
		process.stderr.write("Error: " + err);
		return 1;
	}
	// process.stderr.write( err );
        process.stderr.write( "Image size: "+img.width+"x"+img.height );
	var rows=[];
	for ( var row=0; row < img.height; row++ ) {
		//process.stderr.write( "row #" + row +"\n");
		var rowData = reduceRow( img.data.slice(row*672, (row+1)*672) ) ;
		rows.push(rowData);
		//process.stderr.write( "row "+ row + " : " + rowData.join(" ") +"\n" );
	}

	// compose data for printer
	var syn = String.fromCharCode(0x16); // SYN character
	var esc = String.fromCharCode(27); // ESC character

        //process.stdout.write( esc+"g" ); //density dark
        //var labelLength = 375; // hole-to-hole distance in 1/300ths of inch
        //process.stdout.write( esc+"L"+String.fromCharCode( labelLength / 255 , labelLength % 255 )); //set label length

	var dataArray=[]; //list of buffers we will concat for printer

	dataArray.push(Buffer.from( esc.repeat(86) )); // reset in case of sync error, end print data
	dataArray.push(Buffer.from( esc+"*" )); // restore defaults

	rows.map( function( row) {
		// start this line with "esc D <number of bytes> syn"
		dataArray.push( Buffer.from([27,0x44,row.length,0x16]) );
		// add bytes for this line
		dataArray.push( row );
	});

	dataArray.push(Buffer.from( [ 27, 69 ])); // end job with "esc E" form feed

	var printData = Buffer.concat(dataArray);
	process.stdout.write( printData );

        return;
};

process.stderr.write("Pipe a monochrome png image 672x375 to stdin and pipe stdout to a dymo labelwrite with text driver.\n");

pngparse.parseStream(process.stdin, main) ;
