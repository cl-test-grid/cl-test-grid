//// Copyright (C) 2011 Anton Vodonosov (avodonosov@yandex.ru)
//// See LICENSE for details.

//// This file contains common javascript functions used by cl-test-grid-reports.

function PivotHighlighter($table) {
    this.$table = $table;
    this.$colgroups = $table.find('colgroup');
    var $headerLastRow = this.$table.find('.header-row').last();
    this.$headerLastRowTHs = $headerLastRow.find('th');
    
    // the same for the floating header created by jquery.thead
    var $dummyDivFloat = $($table.parent().next().get(0));
    // assert $dummyDivFloat is DIV with class="dummy-class-for-jquery-thead"
    this.$floatHdrTable = $dummyDivFloat.children('table');
    if (this.$floatHdrTable.size() === 0) {
        throw new Error("the table hasn't been initialized with jquery.thead()");
    }
    var $floatHeaderLastRow = this.$floatHdrTable.find('.header-row').last();
    this.$floatHeaderLastRowTHs = $floatHeaderLastRow.find('th');

    var that = this;
    $table.delegate('td','mouseover mouseleave', function(event) {
        var $td = this;
        that.onMouseOverMouseLeave(event, $td);
    });
}

PivotHighlighter.prototype.onMouseOverMouseLeave = function (event, td) {
      var $td = $(td);
      var $tr = $td.parent();
      var $tdSiblings = $tr.find('td');
      // what clumn to highlight may only be reliably determined
      // based on the current TD index from end of row, because
      // in the beginning of the row we have row headers, which
      // sometimes have one TD distributed over sevelar rows
      // (grepu header), by rowspan="x"; other rows do not have this
      // group header TD (they share common TD for the whole group)
      var indexFromEnd = $tdSiblings.size() - $tdSiblings.index($td);

      var $colgroup = this.$colgroups.eq(this.$colgroups.size() - indexFromEnd);
      var $hdrTH = this.$headerLastRowTHs.eq(this.$headerLastRowTHs.size() - indexFromEnd);
      var $floatHdrTH = this.$floatHeaderLastRowTHs.eq(this.$floatHeaderLastRowTHs.size() - indexFromEnd);

      if (event.type == 'mouseover') {
        $tr.addClass('hover');
        $colgroup.addClass('hover');
        $hdrTH.addClass('hover');
        $floatHdrTH.addClass('hover');
      } else {
        $tr.removeClass('hover');
        $colgroup.removeClass('hover');
        $hdrTH.removeClass('hover');
        $floatHdrTH.removeClass('hover');
      }
}

function setupStatusTooltips() {
    $('.fail-status').closest('td').attr('title', 'Fail');
    $('.ok-status').closest('td').attr('title', 'OK');
    $('.no-resource-status').closest('td').attr('title', 'No Resource');
    $('.crash-status').closest('td').attr('title', 'Crash');
    $('.timeout-status').closest('td').attr('title', 'Timeout');
    var warns = $('.warn-status');
    for (var i = 0; i < warns.length; i++) {
        var $warn = $(warns[i]);
        var text = $warn.text()
        if (text === 'U') {
            $warn.closest('td').attr('title', 'Unexpected OK');
        } else if (text === 'K') {
            $warn.closest('td').attr('title', 'Known Fail');
        } else {
            throw new Error('Unknown status letter: ' + text);
        }
    }
}
