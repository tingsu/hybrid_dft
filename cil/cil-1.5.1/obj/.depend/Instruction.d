    $(OBJDIR)/Instruction.cmo:    $(OBJDIR)/printcil.cmo \
        $(OBJDIR)/Memo.cmo \
        $(OBJDIR)/FindCil.cmo \
        $(OBJDIR)/CilData.cmo \
        $(OBJDIR)/CilCallgraph.cmo  $(OBJDIR)/cil.cmi
    $(OBJDIR)/Instruction.cmx:    $(OBJDIR)/printcil.cmx \
        $(OBJDIR)/Memo.cmx \
        $(OBJDIR)/FindCil.cmx \
        $(OBJDIR)/CilData.cmx \
        $(OBJDIR)/CilCallgraph.cmx  $(OBJDIR)/cil.cmx
