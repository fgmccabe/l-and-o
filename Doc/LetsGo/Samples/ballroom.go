ballroom{
  import go.io.
  import protocol.
  import mail.
  import phemail.

  main(_) ->
      stdout.outLine("Starting...");
      Phems = [ phemail('jill',1), phemail('jane',2), phemail('joan',3), phemail('jenny',4)];
      (F in Phems *> spawn{F.start()});
      Mails = { (mail(N)) .. N in ['fred','jim','peter','alfred','john'] };
      (M in { spawn{ MM.start()} .. MM in Mails} *> waitfor(M));
      stdout.outLine("Reporting...phems...");
      (F in Phems *> stdout.outLine(F.report()));
      stdout.outLine("Reporting...mayls...");
      (M in Mails *> stdout.outLine(M.report())).
}.

      

