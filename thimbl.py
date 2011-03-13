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
import subprocess
import sys
import time

#################################################################

help_text =  """Usage: thimbl CMD [options]

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

setup
   interactively set up your profile

stdin
   post a message by reading from stdin

unfollow ADDRESS
   stop following ADDRESS
"""

#################################################################

def writeln(text):
    print text
        
#################################################################

class MyPlan:
    plan_file = os.path.expanduser('~/.plan')

    def __init__(self):
        # Try to load the cache file, if it exists
        if os.path.isfile(self.plan_file):
            self.data = load(self.plan_file)
        else:
            print 'Configuration was guessed. You should run the commands'
            print "'info' or 'setup' to put in proper values"
            self.data = self.guess_plan()


   
    def __del__(self):
        save(self.data, self.plan_file)


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
            wout('Fingering ' + address)
            try:
                plan = finger_user(address)
                plans.append(plan)
                wout("OK")
            except AttributeError:
                wout('Failed. Skipping')
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
        
        


    def follow(self, nick, address):

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

    def post(self, text):
        'Create a message. Remember to publish() it'
        timefmt = time.strftime('%Y%m%d%H%M%S', time.gmtime())
        message = { 'time' : timefmt, 'text' : text }
        self.data['messages'].append(message)

    def setup(self):
        'Interactively enter user information'
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


    def unfollow(self, address):
        'Remove an address from someone being followed'
        def func(f): return not (f['address'] == address)
        new_followees = filter(func, self.data['following'])
        self.data['following'] = new_followees

        
#################################################################

def dget(d, k, default=None):
    '''Get a value from a dictionary D using key K, 
    returning DEFAULT if key not found'''
    if d.has_key(k):
        return d[k]
    else:
        return default

def finger_user(user_name):
    '''Try to finger a user, and convert the returned plan into a dictionary
    E.g. j = finger_user("dk@telekommunisten.org")
    print j['bio']    
    '''
    args = ['finger', user_name]
    p = subprocess.Popen(args, stdout=subprocess.PIPE)
    output = p.communicate()[0]
    m = re.search('^.*?Plan:\s*(.*)', output, re.M + re.S)
    raw = m.group(1)
    j = json.loads(raw)
    return j




def save(data, filename):
    'Save data to a file as a json file'
    j = json.dumps(data)
    file(filename, 'w').write(j)


         
def load(filename):
    'Load data from a json file'
    s = file(filename, 'r').read()
    return json.loads(s)


    


def main():

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
        
    p = MyPlan()
    cmd = sys.argv[1]
    if cmd =='fetch':
        p.fetch()
    elif cmd == 'follow':
        p.follow(sys.argv[2], sys.argv[3])
    elif cmd == 'following':
        p.following()
    elif cmd == 'info':
        p.info()
    elif cmd == 'help':
        pydoc.pager(help_text)
    elif cmd == 'post':
        p.post(sys.argv[2])
    elif cmd == 'setup':
        p.setup()
    elif cmd == 'stdin':
        text = sys.stdin.read()
        p.post(text)
    elif cmd == 'unfollow':
        p.unfollow(sys.argv[2])
    else:
        print "Unrecognised command: ", cmd


if __name__ == "__main__":
    main()
