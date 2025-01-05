import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Entity} from "../../core/domain/Entity";

export class AllergyId extends Entity<any> {

  get id() : UniqueEntityID {
    return this._id;
  }

  public constructor (id: UniqueEntityID) {
    if(!id) {
      throw new Error("AllergyId must not be null");
    }

    const auxId = id.toString();
    let regex = /^\d+$/;
    if(!regex.test(auxId)) {
      throw new Error("AllergyId must contain only digits");
    }

    super(null, id)
  }

  get value(): number {
    return <number>this.id.toValue();
  }
}
