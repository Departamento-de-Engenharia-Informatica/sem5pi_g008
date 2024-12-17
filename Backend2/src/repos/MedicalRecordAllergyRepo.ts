import {Inject, Service} from "typedi";
import {Document, Model} from "mongoose";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IMedicalRecordAllergyRepo from "../services/IRepos/IMedicalRecordAllergyRepo";
import {IMedicalRecordAllergyPersistence} from "../dataschema/IMedicalRecordAllergyPersistence";


@Service()
export default class MedicalRecordAllergyRepo implements IMedicalRecordAllergyRepo{
  constructor(@Inject('medicalRecordAllergySchema') private medicalRecordAllergySchema: Model<IMedicalRecordAllergyPersistence & Document>,) {
  }

  exists(t: MedicalRecordAllergy): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordAllergy: MedicalRecordAllergy): Promise<MedicalRecordAllergy> {

    const id = await this.getLastId();

    const rawMedicalRecordAllergy: any = MedicalRecordAllergyMapper.toPersistence(medicalRecordAllergy, id);

    const medicalRecordAllergyCreated = await this.medicalRecordAllergySchema.create(rawMedicalRecordAllergy);

    return MedicalRecordAllergyMapper.toDomain(medicalRecordAllergyCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordAllergySchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

}
