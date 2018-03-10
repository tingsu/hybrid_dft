    $(OBJDIR)/MyCriticalEdge.cmo: \
        $(OBJDIR)/MyCilUtility.cmo \
        $(OBJDIR)/FindCil.cmo  $(OBJDIR)/errormsg.cmi \
      $(OBJDIR)/dominators.cmi  $(OBJDIR)/cil.cmi
    $(OBJDIR)/MyCriticalEdge.cmx: \
        $(OBJDIR)/MyCilUtility.cmx \
        $(OBJDIR)/FindCil.cmx  $(OBJDIR)/errormsg.cmx \
      $(OBJDIR)/dominators.cmx  $(OBJDIR)/cil.cmx
