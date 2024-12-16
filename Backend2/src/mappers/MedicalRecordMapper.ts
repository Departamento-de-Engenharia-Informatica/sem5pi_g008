import {Mapper} from "../core/infra/Mapper";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalRecordPersistence} from "../dataschema/IMedicalRecordPersistence";


export class MedicalRecordMapper extends Mapper<MedicalRecord>{

  public static toPersistence (medicalRecord: MedicalRecord, id: any): any {
    return {
      domainId: id,
      medicalRecordConditions: medicalRecord.medicalRecordAllergies,
      medicalRecordAllergies: medicalRecord.medicalRecordAllergies,
      freeText: medicalRecord.freeText
    }
  }

  public static toDomain (medicalRecord: any | Model<IMedicalRecordPersistence & Document> ): MedicalRecord {

    const medicalRecordOrError = MedicalRecord.create(
      medicalRecord,
      new UniqueEntityID(medicalRecord.domainId)
    );

    medicalRecordOrError.isFailure ? console.log(medicalRecordOrError.error) : '';

    return medicalRecordOrError.isSuccess ? medicalRecordOrError.getValue() : null;
  }

}
