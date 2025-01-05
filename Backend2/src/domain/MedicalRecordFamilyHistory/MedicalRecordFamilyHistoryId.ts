import {UniqueEntityID} from "../../core/domain/UniqueEntityID";
import {Entity} from "../../core/domain/Entity";

export class MedicalRecordFamilyHistoryId extends Entity<any> {

  get id() : UniqueEntityID {
    return this._id;
  }

  public constructor (id: UniqueEntityID) {
    if(!id) {
      throw new Error("MedicalRecordFamilyHistoryId must not be null");
    }

    const auxId = id.toString();
    let regex = /^\d+$/;
    if(!regex.test(auxId)) {
      throw new Error("MedicalRecordFamilyHistoryId must contain only digits");
    }

    super(null, id)
  }
}
