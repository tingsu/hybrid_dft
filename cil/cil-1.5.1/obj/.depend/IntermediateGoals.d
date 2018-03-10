    $(OBJDIR)/IntermediateGoals.cmo:   $(OBJDIR)/usedef.cmo \
        $(OBJDIR)/MyReachingDefs.cmo \
        $(OBJDIR)/Instruction.cmo  $(OBJDIR)/errormsg.cmi  $(OBJDIR)/cil.cmi
    $(OBJDIR)/IntermediateGoals.cmx:   $(OBJDIR)/usedef.cmx \
        $(OBJDIR)/MyReachingDefs.cmx \
        $(OBJDIR)/Instruction.cmx  $(OBJDIR)/errormsg.cmx  $(OBJDIR)/cil.cmx
