    $(OBJDIR)/CilCallgraph.cmo: \
        $(OBJDIR)/Profiler.cmo \
        $(OBJDIR)/Module.cmo \
        $(OBJDIR)/Memo.cmo \
        $(OBJDIR)/FindCil.cmo  $(OBJDIR)/errormsg.cmi \
        $(OBJDIR)/CilData.cmo  $(OBJDIR)/cil.cmi
    $(OBJDIR)/CilCallgraph.cmx: \
        $(OBJDIR)/Profiler.cmx \
        $(OBJDIR)/Module.cmx \
        $(OBJDIR)/Memo.cmx \
        $(OBJDIR)/FindCil.cmx  $(OBJDIR)/errormsg.cmx \
        $(OBJDIR)/CilData.cmx  $(OBJDIR)/cil.cmx
