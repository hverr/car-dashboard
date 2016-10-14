$(function() {
  $(".knob").knob({
    draw: function() {
      if (this.$.data('skin') == 'tron') {
        var a = this.angle(this.cv);
        var sa = this.startAngle
        var sat = this.startAngle
        var ea;
        var eat = sat + a;
        var r = true;

        this.g.lineWidth = this.lineWidth;
        this.o.cursor && (sat = eat - 0.3) && (eat = eat + 0.3);

        if (this.o.displayPrevious) {
          ea = this.startAngle + this.angle(this.value);
          this.o.cursor && (sa = ea - 0.3) && (ea = ea + 0.3);
          this.g.beginPath();
          this.g.strokeStyle = this.previousColor;
          this.g.arc(this.xy, this.xy, this.radius - this.lineWidth, sa, ea, false);
          this.g.stroke();
        }

        this.g.beginPath();
        this.g.strokeStyle = r ? this.o.fgColor : this.fgColor;
        this.g.arc(this.xy, this.xy, this.radius - this.lineWidth, sat, eat, false);
        this.g.stroke();

        this.g.lineWidth = 2;
        this.g.beginPath();
        this.g.strokeStyle = this.o.fgColor;
        this.g.arc(this.xy, this.xy,
          this.radius - this.lineWidth + 1 + this.lineWidth * 2 / 3,
          0, 2 * Math.PI, false);
        this.g.stroke();

        return false;
      }
    }, // end draw function
  }); // end initialize knob
});
