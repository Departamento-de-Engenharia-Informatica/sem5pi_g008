import {Mapper} from "../core/infra/Mapper";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";


export class MedicalRecordMapper extends Mapper<MedicalRecord>{

  public static toPersistence (medicalRecord: MedicalRecord, id: any): any {
    return {
      domainId: id,
      medicalRecordConditions: medicalRecord.medicalRecordAllergies,
      medicalRecordAllergies: medicalRecord.medicalRecordAllergies,
      freeText: medicalRecord.freeText
    }
  }

}
