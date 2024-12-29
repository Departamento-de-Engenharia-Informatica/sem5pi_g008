import {Repo} from "../../core/infra/Repo";
import {MedicalRecordFreeText} from "../../domain/MedicalRecordFreeText/MedicalRecordFreeText";
import {MedicalRecordFamilyHistory} from "../../domain/MedicalRecordFamilyHistory/MedicalRecordFamilyHistory";

export default interface IMedicalRecordFamilyHistoryRepo extends Repo<MedicalRecordFamilyHistory> {
  save(medicalRecordFamilyHistory: MedicalRecordFamilyHistory): Promise<MedicalRecordFamilyHistory>;
  saveFamilyHistory(medicalRecord: Promise<any>, familylist: any[]): any;
}
