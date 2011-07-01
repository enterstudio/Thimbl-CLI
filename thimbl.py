'''thimbl.py - Command-line python tools '''

import copy
import cStringIO
import datetime
import getpass
import json
import pdb
import optparse
import os
import platform
import pydoc
import re
import socket
import subprocess
import sys
import time
import traceback
import types

#################################################################

help_text =  """Usage: thimbl CMD [options]

change
   Interactively change the details of your profile. Use `reset' instead 
   if your plan is hopelessly mangled.

fetch 
   download and print all messages

follow NICK ADDRESS
   follow someone at ADDRESS, giving them a nickname NICK
   E.g. thimbl follow dk dmytri@thimbl.tk

following
   print a list of all people being followed

info
   print info about your account

post MSG
   create a post. e.g. thimbl post "First post"

reset
   !DANGER WILL ROBINSON. DATA WILL BE LOST!
   Hard reset the plan file. Useful if it becomes corrupt, and you need to
   reset it to a default guessed state.
   

stdin
   post a message by reading from stdin

unfollow ADDRESS
   stop following ADDRESS
"""

#################################################################

class ThimblException(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return 'ThimblException: ' + self.value

#################################################################

def writeln(text):
    print text
        
#################################################################

class FetchError(Exception):
    def __init__(self, value):
        self.value = value
    
    def __str_(self):
        return repr(self.value)

#################################################################
class MyPlan:
    plan_file = os.path.expanduser('~/.plan')

    def __init__(self, reset = False):
        # Try to load the cache file, if it exists
        self.prepost = ['', '']
        if os.path.isfile(self.plan_file) and not reset:
            self.data = load(self.plan_file, keep_prepost=self.prepost)
        else:
            print 'Configuration was guessed. You should run the commands'
            print "'info' or 'setup' to put in proper values"
            self.data = self.guess_plan()


   
    def __del__(self):
        # This class wont have a data attribute when the plan fails to load
        if hasattr(self, 'data'):
            save(self.data, self.plan_file,
                 prefix=self.prepost[0],
                 postfix=self.prepost[1])


    def guess_plan(self):
        'Create a blank config cache, populating it with sensible defaults'

        host = platform.node()
        name = getpass.getuser() # user name
        email = '{0}@{1}'.format(name, host)
        address = email

        properties =  {}
        properties['website'] = 'http://' + host
        properties['mobile']  = 'Mobile withheld'
        properties['email'] = email

        plan = {}
        plan['address'] = address
        plan['name'] = name
        plan['messages'] = []
        plan['replies'] = {}
        plan['following'] = []
        plan['properties'] = properties

        return plan


    def fetch(self, wout = writeln):
        '''Retrieve all the plans of the people I am following, 
        and print them in reverse chronological order'''

        # retrieve plans
        plans = []
        for following in self.data['following']:
            address = following['address']
            try:
                wout('Fetching {0}'.format(address))
                plan = finger_user(address, wout)
                plans.append(plan)
                wout("... OK")
            except FetchError as e:
                wout('... Failed. Skipping. Cause: {0}'.format(e.value))
                continue

        # accumulate messages
        messages = []
        for plan in plans:
            if not plan.has_key('address'): continue
            address = plan['address']
            if not plan.has_key('messages'): continue
            for m in plan['messages']:
                message = {'address' : address}
                if not m.has_key('text'): continue
                message['text'] = m['text']
                if not m.has_key('time'): continue
                message['time'] = m['time']
                messages.append(message)
                
        messages.sort(key = lambda x: x['time'])
        
        # print messages
        for msg in messages:
            # format time
            t = str(msg['time'])
            tlist = map(int, [t[:4], t[4:6], t[6:8], t[8:10], 
                              t[10:12], t[12:14]])
            tstruct = apply(datetime.datetime, tlist)
            ftime = tstruct.strftime('%Y-%m-%d %H:%M:%S')

            text = '{0}  {1}\n{2}\n\n'.format(ftime, msg['address'], 
                                              msg['text'].encode('utf-8'))
            wout(text)
        
        


    def follow(self, nick = None, address = None):

        if (nick is None) or (address is None):
            try:
                nick = sys.argv[2]
                address = sys.argv[3]
            except IndexError:
                raise ThimblException("Error in FOLLOW. Couldn't find NICK and ADDRESS.")


        # rebuild the list of followees, removing duplicate addresses
        followees = []
        for f in self.data['following']:
            if f['address'] == address:
                print "Dropping dupe address"
            else:
                followees.append(f)
                
        # now add the address back in
        followees.append({ 'nick' : nick, 'address' : address })

        self.data['following'] = followees

    def following(self):
        'Who am I following?'
        followees = self.data['following']
        followees.sort(key = lambda x: x['nick'])
        for f in followees:
            print '{0:5} {1}'.format(f['nick'], f['address'])

    def info(self):
        'Print some information about thimbl-cli'
        print "plan_file:      " + self.plan_file
        print "name:           " + self.data['name']
        print "address:        " + self.data['address']
        props =  self.data['properties']
        print "email:          " + props['email']
        print "mobile:         " + props['mobile']
        print "website:        " + props['website']

    def post(self, text = None):

        if text is None:
            try:
                text = sys.argv[2]
            except IndexError:
                raise ThimblException("Error in POST. Coudldn't find TEXT.")

        'Create a message. Remember to publish() it'
        timefmt = time.strftime('%Y%m%d%H%M%S', time.gmtime())
        message = { 'time' : timefmt, 'text' : text }
        self.data['messages'].append(message)

    def change(self):
        'Interactively change user information'
        def getval(v, prompt):
            while True:
                print prompt + '=' + v+ ' [Accept (default)/Change/Erase]? '
                inp = raw_input()
                if inp in ['A', 'a', '']:
                    return v
                elif inp in ['C', 'c']:
                    print 'Input new value: ',
                    v = raw_input()
                    return v
                elif inp in ['E', 'e']:
                    return ''
                else:
                    print "Didn't understand your response"

        for k in ['name', 'address']:
            self.data[k] = getval(self.data[k], k)

        for k in ['website', 'mobile', 'email']:
            self.data['properties'][k] = getval(self.data['properties'][k], k)


    def unfollow(self, address = None):
        'Remove an address from someone being followed'
        if address is None:
            try:
                address = sys.argv[2]
            except IndexError:
                raise ThimblException("Error in UNFOLLOW. ADDRESS unspecified.")

        def func(f): return not (f['address'] == address)
        new_followees = filter(func, self.data['following'])
        self.data['following'] = new_followees

    def stdin(self):
        text = sys.stdin.read()
        self.post(text)

        
#################################################################

def dget(d, k, default=None):
    '''Get a value from a dictionary D using key K, 
    returning DEFAULT if key not found'''
    if d.has_key(k):
        return d[k]
    else:
        return default

# consider the following function deprecated, but keep it
# around for awhile in case it comes in handy. It is certainly useful
# as a demo on how the piping mechanism works
def finger_user_deprecated(user_name):
    '''Try to finger a user, and convert the returned plan into a dictionary
    E.g. j = finger_user("dk@telekommunisten.org")
    print j['bio']    
    '''
    args = ['finger', user_name]
    p = subprocess.Popen(args, stdout=subprocess.PIPE)
    output = p.communicate()[0]
    m = re.search('^.*?Plan:\s*(\{.*)$', output, re.M + re.S)
    raw = m.group(1)
    j = json.loads(raw)
    return j


def finger(name, host, wout = writeln):

    # preliminary check we can connect to host server
    # it saves time if there's a host lookup failure
    try:
        socket.gethostbyname(host)
    except socket.gaierror:
        raise FetchError("Host '{0}' not found".format(host))


    timeout = 10
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.settimeout(timeout)
        s.connect((host, 79))
        s.send(name + "\r\n")
        result = ""
        while 1:
            data = s.recv(4096)        
            if len(data) == 0: break
            result += data
        s.close()
    except socket.gaierror:
        raise FetchError("socket.gaierror (2)")
    except socket.timeout:
        raise FetchError("Timeout after {0} seconds".format(timeout))


    # check that we found the user
    if result.find('no such user') <> -1:
        raise FetchError("No user '{0}'".format(name))

    wout("... Data received")
    return result

def finger_user(user_name, wout = writeln):
    '''Try to finger a user, and convert the returned plan into a dictionary
    E.g. j = finger_user("dk@telekommunisten.org")
    print j['bio']    
    '''
    
    m = re.search("(.+)@(.+)", user_name)
    if m is None:
        msg = "User/host decoding failure for {0}".format(user_name)
        raise FetchError(msg)
    name, host  = m.groups()


    output = finger(name, host, wout)
    m = re.search('^.*?Plan:\s*(\{.*)', output, re.M + re.S)
    if m is None:
        raise FetchError("No plan")
    raw = m.group(1)
    try:
        j = json.loads(raw)
    except Exception as e:
        raise FetchError("Json reports: {0}".format(e))
    return j






def save(data, filename, prefix='', postfix=''):
    'Save data to a file as a json file'
    j = json.dumps(data)
    file(filename, 'w').write(prefix+j+postfix)


         
def load(filename, keep_prepost=None):
    'Load data from a json file'
    s = file(filename, 'r').read()
    try:
        m = re.search('^(.*?)(\{.*\})(.*?)$', s, re.M + re.S)
        j = json.loads(m.group(2))
        if keep_prepost is not None:
            keep_prepost[0:0] = [m.group(1), m.group(3)]
    except:
        raise ThimblException("Plan file can't be decoded. It is not valid JSON. Are you using OS X?")
    return j


    


def uncaught_main():

    #parser.add_option("-f", "--file", dest="filename",
    #help="write report to FILE", metavar="FILE")
    #parser.add_option("-q", "--quiet",
    #action="store_false", dest="verbose", default=True,
    #help="don't print status messages to stdout")
    #parser.add_option

    num_args = len(sys.argv) - 1
    if num_args < 1 :
        print "No command specified. Try help"
        return

    cmd = sys.argv[1]
    if cmd == 'help':
        pydoc.pager(help_text)
        return

    p = MyPlan(cmd == 'reset')

    if cmd == 'reset': return # we don't need to do anything

    if cmd in ['change', 'fetch', 'follow', 'following', 
               'info', 'post', 'stdin', 'unfollow']:
        eval_cmd = 'p.{0}()'.format(cmd)
        eval(eval_cmd)
    else:
        print "Unrecognised command: ", cmd
    return


def main():
    try:
        uncaught_main()
    except ThimblException as e:
        print e
        print "Type 'thimbl help' for some tips."

if __name__ == "__main__":
    main()
