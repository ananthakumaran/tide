var x = 'string';

var len = x.length;

len.substring(0, 1);

var y = Game;

var obj: Object;

@readonly
export class Person {
    name: string;
    isAdmin: boolean;

    constructor(name:string, admin: boolean) {
        this.name = name;
        this.isAdmin = admin;
    }
}


function readonly<TFunction extends Function>(Target: TFunction): TFunction {
    let newConstructor = function () {
        Target.apply(this);
        Object.freeze(this);
    };

    newConstructor.prototype = Object.create(Target.prototype);
    newConstructor.prototype.constructor = Target;
    return <any> newConstructor;
}
